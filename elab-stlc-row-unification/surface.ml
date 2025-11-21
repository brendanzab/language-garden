(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type span =
  Lexing.position * Lexing.position

(** Spanned nodes *)
type 'a spanned = {
  span : span;
  data : 'a;
}

(** Labels that distinguish variant cases, record fields, etc. *)
type label = string spanned

(** Names that bind definitions or parameters *)
type binder = string option spanned

(** Types in the surface language *)
type ty =
  ty_data spanned

and ty_data =
  | Name of string
  | Fun_type of ty * ty
  | Record_type of (label * ty) list
  | Variant_type of (label * ty) list
  | Placeholder

type pattern =
  pattern_data spanned

and pattern_data =
  (* | Name of string *)
  (* | Placeholder *)
  (* | Int_lit of int *)
  | Variant_lit of label * binder

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Record_lit of (label * tm) list
  | Variant_lit of label * tm
  | Int_lit of int
  | App of tm * tm
  | Proj of tm * label
  | Match of tm * (pattern * tm) list
  | If_then_else of tm * tm * tm
  | Infix of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Prefix of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


(** {1 User-facing diagnostics} *)

(** The severity level of a diagnostic message. *)
module Severity = struct

  type t =
    | Warning
    | Error

  let to_string (s : t) : string =
    match s with
    | Warning -> "warning"
    | Error -> "error"

end

(** A diagnostic message that should be reported to the programmer. *)
module Diagnostic = struct

  type t = {
    severity : Severity.t;
    span : span;
    message : string;
    details : string list;
  }

  let warning ?(details = ([] : string list)) (span : span) (message : string) : t =
    { severity = Warning; span; message; details }

  let error ?(details = ([] : string list)) (span : span) (message : string) : t =
    { severity = Error; span; message; details }

end


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check_ty : ty -> Core.ty option * Diagnostic.t list
  val check_tm : tm -> Core.ty -> Core.tm option * Diagnostic.t list
  val infer_tm : tm -> (Core.tm * Core.ty) option * Diagnostic.t list

end = struct

  module Label_map = Core.Label_map


  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    tys : (string option * Core.ty) Core.env;
    (** A stack of bindings currently in scope *)

    metas : (span * string * Core.meta) Dynarray.t;
    (** A list of the metavariables that have been inserted during elaboration.
        This will be used to generate a list of unsolved metavariables once
        elaboration is complete. *)

    row_metas : Core.row_meta Dynarray.t;
    (** A list of the row metavariables that have been inserted during
        elaboration. Any unsolved row metavariables will be defaulted to their
        row constraint at the end of elaboration. *)

    diagnostics : Diagnostic.t Dynarray.t;
    (** Diagnostic messages recorded during elaboration. *)
  }

  (** The empty context *)
  let empty () : context = {
    tys = [];
    metas = Dynarray.create ();
    row_metas = Dynarray.create ();
    diagnostics = Dynarray.create ();
  }

  (** Extend the context with a new binding *)
  let extend (ctx : context) (name : string option) (ty : Core.ty) : context = {
    ctx with
    tys = (name, ty) :: ctx.tys;
  }

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
    ctx.tys |> List.find_mapi @@ fun index (name', ty) ->
      match Some name = name' with
      | true -> Some (index, ty)
      | false -> None

  (** Generate a fresh metavariable *)
  let fresh_meta (ctx : context) (span: span) (info : string) : Core.ty =
    let m = Core.fresh_meta () in
    Dynarray.add_last ctx.metas (span, info, m);
    Core.Meta_var m

  (** Generate a fresh row metavariable *)
  let fresh_row_meta (ctx : context) (row: Core.ty Label_map.t) : Core.row_ty =
    let rm = Core.fresh_row_meta row in
    Dynarray.add_last ctx.row_metas rm;
    Row_meta_var rm


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error

  (** Record a diagnostic in the elaboration context *)
  let report (ctx : context) (diagnostic : Diagnostic.t) =
    Dynarray.add_last ctx.diagnostics diagnostic

  (** Report a warning to the programmer *)
  let warning (type a) ?(details : string list option) (ctx : context) (span : span) : (a, Format.formatter, unit, unit) format4 -> a =
    Format.kasprintf @@ fun message ->
      report ctx (Diagnostic.warning span message ?details)

  (** Terminate elaboration, reporting an error *)
  let error (type a b) ?(details : string list option) (ctx : context) (span : span) : (a, Format.formatter, unit, b) format4 -> a =
    Format.kasprintf @@ fun message ->
      report ctx (Diagnostic.error span message ?details);
      raise Error

  let unify_tys (ctx : context) (span : span) ~(found : Core.ty) ~(expected : Core.ty) =
    try Core.unify_tys found expected with
    | Core.Infinite_type _ -> error ctx span "infinite type"
    | Core.Infinite_row_type _ -> error ctx span "infinite row type"
    | Core.Mismatched_types (_, _)
    | Core.Mismatched_row_types (_, _) ->
        error ctx span "mismatched types"
          ~details:[
            Format.asprintf "@[<v>@[expected: %t@]@ @[   found: %t@]@]"
              (Core.pp_ty expected)
              (Core.pp_ty found);
          ]


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ctx : context) (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Core.Bool_type
    | Name "Int" -> Core.Int_type
    | Name name -> error ctx ty.span "unbound type `%s`" name
    | Fun_type (ty1, ty2) -> Core.Fun_type (check_ty ctx ty1, check_ty ctx ty2)
    | Record_type entries -> Core.Record_type (check_ty_entries ctx entries)
    | Variant_type entries -> Core.Variant_type (check_ty_entries ctx entries)
    | Placeholder -> fresh_meta ctx ty.span "placeholder"

  and check_ty_entries ctx entries =
    let rec go acc entries =
      match entries with
      | [] -> acc
      | (label, ty) :: entries ->
          begin match Label_map.mem label.data acc with
          | true -> error ctx label.span "duplicate label `%s`" label.data
          | false -> go (Label_map.add label.data (check_ty ctx ty) acc) entries
          end
    in
    Row_entries (go Label_map.empty entries)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data, Core.force_ty ty with
    | Let (def_name, params, def_body_ty, def_body, body), body_ty ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend ctx def_name.data def_ty) body body_ty in
        Core.Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body), ty ->
        check_fun_lit ctx params body ty

    | Variant_lit (label, tm), Core.Variant_type (Core.Row_entries row) ->
        begin match Label_map.find_opt label.data row with
        | Some elem_ty ->
            Core.Variant_lit (label.data, check_tm ctx tm elem_ty, ty)
        | None ->
            error ctx label.span "unexpected variant `%s` in type `%t`"
              label.data
              (Core.pp_ty ty)
        end

    | Match (head, clauses), body_ty ->
        check_tm_match ctx head clauses body_ty

    | If_then_else (head, tm1, tm2), ty ->
        let head = check_tm ctx head Core.Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Core.Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        (* NOTE: We could add some subtyping coercions here *)
        unify_tys ctx tm.span ~found:ty' ~expected:ty;
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (index, ty) -> Core.Var index, ty
        | None when name = "true" -> Core.Bool_lit true, Core.Bool_type
        | None when name = "false" -> Core.Bool_lit false, Core.Bool_type
        | None -> error ctx tm.span "unbound name `%s`" name
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend ctx def_name.data def_ty) body in
        Core.Let (def_name.data, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        check_tm ctx tm ty, ty

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | Record_lit entries ->
        let rec go acc entries =
          match entries with
          | [] -> acc
          | (label, tm) :: entries ->
              match Label_map.mem label.data acc with
              | true -> error ctx label.span "duplicate label `%s`" label.data
              | false -> go (Label_map.add label.data (infer_tm ctx tm) acc) entries
        in
        let entries = go Label_map.empty entries in
        Core.Record_lit (Label_map.map fst entries),
        Core.Record_type (Row_entries (Label_map.map snd entries))

    | Variant_lit (label, elem_tm) ->
        let elem_tm, elem_ty = infer_tm ctx elem_tm in
        let row = Label_map.singleton label.data elem_ty in
        let ty = Core.Variant_type (fresh_row_meta ctx row) in
        Core.Variant_lit (label.data, elem_tm, ty), ty

    | Int_lit i ->
        Core.Int_lit i, Core.Int_type

    | App ({ span = head_span; _ } as head, arg) ->
        let head, head_ty = infer_tm ctx head in
        begin match Core.force_ty head_ty with
        | Core.Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty
        | Meta_var _ as head_ty ->
            let param_ty = fresh_meta ctx head_span "function parameter type" in
            let body_ty = fresh_meta ctx head_span "function return type" in
            unify_tys ctx head_span ~found:head_ty ~expected:(Core.Fun_type (param_ty, body_ty));
            let arg = check_tm ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty
        | _ -> error ctx arg.span "unexpected argument"
        end

    | Match (head, clauses) ->
        let body_ty = fresh_meta ctx tm.span "match clauses" in
        check_tm_match ctx head clauses body_ty, body_ty

    | Proj (head, label) ->
        let head_span = head.span in
        let head, head_ty = infer_tm ctx head in

        begin match Core.force_ty head_ty with
        | Core.Record_type (Core.Row_entries row) when Label_map.mem label.data row ->
            Core.Record_proj (head, label.data), Label_map.find label.data row
        | Core.Record_type (Core.Row_meta_var _) | Core.Meta_var _ as head_ty ->
            let field_ty = fresh_meta ctx head_span "record field" in
            let row = Label_map.singleton label.data field_ty in
            unify_tys ctx head_span ~found:head_ty ~expected:(Record_type (fresh_row_meta ctx row));
            Core.Record_proj (head, label.data), field_ty
        | head_ty ->
            error ctx head_span "unknown field `%s`" label.data
              ~details:[
                Format.asprintf "@[<v>@[expected: { .. %s : _ }@]@ @[   found: %t@]@]"
                  label.data
                  (Core.pp_ty head_ty);
              ]
        end

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Core.Bool_type in
        let ty = fresh_meta ctx tm.span "if expression branches" in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Core.Bool_elim (head, tm1, tm2), ty

    | Infix (`Eq, tm1, tm2) ->
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        unify_tys ctx tm.span ~found:ty2 ~expected:ty1;
        begin match Core.force_ty ty1 with
        | Core.Bool_type -> Core.Prim_app (Prim.Bool_eq, [tm1; tm2]), Core.Bool_type
        | Core.Int_type -> Core.Prim_app (Prim.Int_eq, [tm1; tm2]), Core.Bool_type
        | ty ->
            error ctx tm.span "@[<h>cannot compare operands of type `%t`@]"
              (Core.pp_ty ty)
              ~details:[
                Format.asprintf "expected `%t` or `%t`"
                  (Core.pp_ty Core.Bool_type)
                  (Core.pp_ty Core.Int_type);
              ]
        end

    | Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Core.Int_type in
        let tm2 = check_tm ctx tm2 Core.Int_type in
        Core.Prim_app (prim, [tm1; tm2]), Core.Int_type

    | Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Core.Int_type in
        Core.Prim_app (Prim.Int_neg, [tm]), Core.Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    match params, Core.force_ty ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Core.Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit (extend ctx name.data param_ty) params body body_ty in
        Core.Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Core.Fun_type (param_ty', body_ty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        unify_tys ctx param_ty_span ~found:param_ty ~expected:param_ty';
        let body = check_fun_lit (extend ctx name.data param_ty) params body body_ty in
        Core.Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, Core.Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params None body in
        unify_tys ctx name.span ~found:ty' ~expected:ty;
        tm'
    | (name, _) :: _, _ ->
        error ctx name.span "unexpected parameter"

  (** Elaborate a function literal, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta ctx name.span "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let body, body_ty = infer_fun_lit (extend ctx name.data param_ty) params body_ty body in
        Core.Fun_lit (name.data, param_ty, body), Core.Fun_type (param_ty, body_ty)

  (** Elaborate a pattern match, checking the clause bodies against an expected type. *)
  and check_tm_match (ctx : context) (head : tm) (clauses : (pattern * tm) list) (body_ty : Core.ty) : Core.tm =
    let head_span = head.span in
    let head, head_ty = infer_tm ctx head in

    (* TODO: Proper match compilation *)
    match Core.force_ty head_ty with
    | Core.Variant_type (Core.Row_entries row) ->
        (* Iterate through clauses, accumulating clauses *)
        let clauses =
          clauses |> ListLabels.fold_left
            ~init:Label_map.empty
            ~f:(fun clauses ({ data = Variant_lit (label, name); _}, body_tm : pattern * _) ->
              if Label_map.mem label.data clauses then begin
                warning ctx label.span "redundant case pattern";
                clauses
              end else
                match Label_map.find_opt label.data row with
                | Some param_ty ->
                    let body_tm = check_tm (extend ctx name.data param_ty) body_tm body_ty in
                    Label_map.add label.data (name.data, body_tm) clauses
                | None ->
                    error ctx label.span "unexpected pattern")
        in
        (* Check that labels in the clauses match the labels in the row *)
        let missing_clauses =
          Label_map.to_seq row
          |> Seq.filter (fun (label, _) -> not (Label_map.mem label clauses))
          |> List.of_seq
        in
        if List.is_empty missing_clauses then
          (* Return the clauses *)
          Core.Variant_elim (head, clauses)
        else
          error ctx head_span "non-exhaustive match"
            ~details:(missing_clauses |> List.map (fun (label, _) ->
              Format.sprintf "missing case pattern `%s`" label))

    | head_ty ->
        (* Build up the clauses and the row from the clauses *)
        let clauses, row =
          clauses |> ListLabels.fold_left
            ~init:(Label_map.empty, Label_map.empty)
            ~f:(fun (clauses, row) ({ data = Variant_lit (label, name); _ }, body_tm : pattern * _) ->
              if Label_map.mem label.data clauses then begin
                warning ctx label.span "redundant case pattern";
                clauses, row
              end else
                let param_ty = fresh_meta ctx name.span "pattern binder" in
                let body_tm = check_tm (extend ctx name.data param_ty) body_tm body_ty in
                Label_map.add label.data (name.data, body_tm) clauses,
                Label_map.add label.data param_ty row)
        in
        (* Unify variant type with head type *)
        unify_tys ctx head_span ~found:head_ty ~expected:(Core.Variant_type (Core.Row_entries row));
        (* Return the clauses *)
        Core.Variant_elim (head, clauses)


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : context -> a) : a option * Diagnostic.t list =
    let ctx = empty () in

    match prog ctx with
    | result ->
        (* Collect ambiguities *)
        ctx.metas |> Dynarray.iter begin fun (span, info, m) ->
          match !m with
          | Core.Unsolved _ ->
              report ctx @@ Diagnostic.error span ("ambiguous " ^ info)
          | Core.Solved _ -> ()
        end;

        (* Default unsolved rows to fixed row types *)
        ctx.row_metas |> Dynarray.iter begin fun m ->
          match !m with
          | Core.Unsolved_row (_, row) ->
              m := Core.Solved_row (Core.Row_entries row);
          | Core.Solved_row _ -> ()
        end;

        Some result, Dynarray.to_list ctx.diagnostics

    | exception Error ->
        None, Dynarray.to_list ctx.diagnostics


  (** {2 Public API} *)

  let check_ty (ty : ty) : Core.ty option * Diagnostic.t list =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : tm) (ty : Core.ty) : Core.tm option * Diagnostic.t list =
    run_elab (fun ctx -> check_tm ctx tm ty)

  let infer_tm (tm : tm) : (Core.tm * Core.ty) option * Diagnostic.t list =
    run_elab (fun ctx -> infer_tm ctx tm)

end
