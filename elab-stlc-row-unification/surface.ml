(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Labels that distinguish variant cases, record fields, etc. *)
type label = string located

(** Names that bind definitions or parameters *)
type binder = string option located

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | Fun_type of ty * ty
  | Record_type of (label * ty) list
  | Variant_type of (label * ty) list
  | Placeholder

type pattern =
  pattern_data located

and pattern_data =
  (* | Name of string *)
  (* | Placeholder *)
  (* | Int_lit of int *)
  | Variant_lit of label * binder

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Record_lit of (label * tm) list
  | Variant_lit of label * tm
  | Int_lit of int
  | Bool_lit of bool
  | App of tm * tm
  | Proj of tm * label
  | Match of tm * (pattern * tm) list
  | If_then_else of tm * tm * tm
  | Op2 of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


module Diagnostic = struct

  type severity =
    | Warning
    | Error

  type t = {
    loc : loc;
    severity : severity;
    message : string;
  }

  let error loc message = { loc; severity = Error; message }
  let warning loc message = { loc; severity = Warning; message }

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

    metas : (loc * string * Core.meta) Dynarray.t;
    (** A list of the metavariables that have been inserted during elaboration.
        This will be used to generate a list of unsolved metavariables once
        elaboration is complete. *)

    row_metas : Core.row_meta Dynarray.t;
    (** A list of the row metavariables that have been inserted during
        elaboration. Any unsolved row metavariables will be defaulted to their
        row constraint at the end of elaboration. *)

    diagnostics : Diagnostic.t Dynarray.t;
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
  let fresh_meta (ctx : context) (loc: loc) (info : string) : Core.ty =
    let m = Core.fresh_meta () in
    Dynarray.add_last ctx.metas (loc, info, m);
    Meta_var m

  (** Generate a fresh row metavariable *)
  let fresh_row_meta (ctx : context) (row: Core.ty Label_map.t) : Core.row_ty =
    let rm = Core.fresh_row_meta row in
    Dynarray.add_last ctx.row_metas rm;
    Row_meta_var rm


  (** {2 Elaboration errors} *)

  let report (ctx : context) (d : Diagnostic.t) : unit =
    Dynarray.add_last ctx.diagnostics d

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors. This is normal, and should be rendered
      nicely to the programmer. *)
  exception Error of (loc * string)

  (** Raises an {!Error} exception *)
  let error (type a) (loc : loc) (message : string) : a =
    raise (Error (loc, message))

  let unify_tys (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
    try Core.unify_tys ty1 ty2 with
    | Core.Infinite_type _ -> error loc "infinite type"
    | Core.Infinite_row_type _ -> error loc "infinite row type"
    | Core.Mismatched_types (_, _)
    | Core.Mismatched_row_types (_, _) ->
        error loc
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[found: %t@]@]"
            (Core.pp_ty ty1)
            (Core.pp_ty ty2))


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
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name -> error ty.loc (Format.asprintf "unbound type `%s`" name)
    | Fun_type (ty1, ty2) -> Fun_type (check_ty ctx ty1, check_ty ctx ty2)
    | Record_type entries -> Record_type (check_ty_entries ctx entries)
    | Variant_type entries -> Variant_type (check_ty_entries ctx entries)
    | Placeholder -> fresh_meta ctx ty.loc "placeholder"

  and check_ty_entries ctx entries =
    let rec go acc entries =
      match entries with
      | [] -> acc
      | (label, ty) :: entries ->
          begin match Label_map.mem label.data acc with
          | true -> error label.loc (Format.asprintf "duplicate label `%s`" label.data)
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
        Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body), ty ->
        check_fun_lit ctx params body ty

    | Variant_lit (label, tm), Variant_type (Row_entries row) ->
        begin match Label_map.find_opt label.data row with
        | Some elem_ty ->
            Variant_lit (label.data, check_tm ctx tm elem_ty, ty)
        | None ->
            error label.loc
              (Format.asprintf "unexpected variant `%s` in type `%t`"
                label.data
                (Core.pp_ty ty))
        end

    | Match (head, clauses), body_ty ->
        check_tm_match ctx head clauses body_ty

    | If_then_else (head, tm1, tm2), ty ->
        let head = check_tm ctx head Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        (* NOTE: We could add some subtyping coercions here *)
        unify_tys tm.loc ty ty';
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (index, vty) -> Var index, vty
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend ctx def_name.data def_ty) body in
        Let (def_name.data, def_ty, def, body), body_ty

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
              | true -> error label.loc (Format.asprintf "duplicate label `%s`" label.data)
              | false -> go (Label_map.add label.data (infer_tm ctx tm) acc) entries
        in
        let entries = go Label_map.empty entries in
        Record_lit (Label_map.map fst entries),
        Record_type (Row_entries (Label_map.map snd entries))

    | Variant_lit (label, elem_tm) ->
        let elem_tm, elem_ty = infer_tm ctx elem_tm in
        let row = Label_map.singleton label.data elem_ty in
        let ty = Core.Variant_type (fresh_row_meta ctx row) in
        Variant_lit (label.data, elem_tm, ty), ty

    | Int_lit i ->
        Int_lit i, Int_type

    | Bool_lit b ->
        Bool_lit b, Bool_type

    | App (head, arg) ->
        let head_loc = head.loc in
        let head, head_ty = infer_tm ctx head in

        begin match Core.force_ty head_ty with
        | Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Fun_app (head, arg), body_ty
        | Meta_var _ as head_ty ->
            let param_ty = fresh_meta ctx head_loc "function parameter type" in
            let body_ty = fresh_meta ctx head_loc "function return type" in
            unify_tys head_loc (Fun_type (param_ty, body_ty)) head_ty;
            let arg = check_tm ctx arg param_ty in
            Fun_app (head, arg), body_ty
        | head_ty ->
            error head_loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: function@]@ @[found: %t@]@]"
                (Core.pp_ty head_ty))
        end

    | Match (head, clauses) ->
        let body_ty = fresh_meta ctx tm.loc "match clauses" in
        check_tm_match ctx head clauses body_ty, body_ty

    | Proj (head, label) ->
        let head_loc = head.loc in
        let head, head_ty = infer_tm ctx head in

        begin match Core.force_ty head_ty with
        | Record_type (Row_entries row) when Label_map.mem label.data row ->
            Record_proj (head, label.data), Label_map.find label.data row
        | Record_type (Row_meta_var _) | Meta_var _ as head_ty ->
            let field_ty = fresh_meta ctx head_loc "record field" in
            let row = Label_map.singleton label.data field_ty in
            unify_tys head_loc (Record_type (fresh_row_meta ctx row)) head_ty;
            Record_proj (head, label.data), field_ty
        | head_ty ->
            error head_loc
              (Format.asprintf "@[<v 2>@[unknown field `%s`:@]@ @[found: %t@]@]"
                label.data
                (Core.pp_ty head_ty))
        end

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let ty = fresh_meta ctx tm.loc "if expression branches" in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2), ty

    | Op2 (`Eq, tm0, tm1) ->
        let tm0, ty0 = infer_tm ctx tm0 in
        let tm1, ty1 = infer_tm ctx tm1 in
        unify_tys tm.loc ty0 ty1;
        begin match Core.force_ty ty0 with
        | Bool_type -> Prim_app (Bool_eq, [tm0; tm1]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm0; tm1]), Bool_type
        | ty -> error tm.loc (Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty))
        end

    | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm0 = check_tm ctx tm0 Int_type in
        let tm1 = check_tm ctx tm1 Int_type in
        Prim_app (prim, [tm0; tm1]), Int_type

    | Op1 (`Neg, tm) ->
        let tm = check_tm ctx tm Int_type in
        Prim_app (Int_neg, [tm]), Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    match params, Core.force_ty ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit (extend ctx name.data param_ty) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty_loc = param_ty.loc in
        let param_ty = check_ty ctx param_ty in
        unify_tys param_ty_loc param_ty param_ty';
        let body = check_fun_lit (extend ctx name.data param_ty) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params None body in
        unify_tys name.loc ty ty';
        tm'
    | (name, _) :: _, _ ->
        error name.loc "unexpected parameter"

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
          | None -> fresh_meta ctx name.loc "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let body, body_ty = infer_fun_lit (extend ctx name.data param_ty) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)

  (** Elaborate a pattern match, checking the clause bodies against an expected type. *)
  and check_tm_match (ctx : context) (head : tm) (clauses : (pattern * tm) list) (body_ty : Core.ty) : Core.tm =
    let head_loc = head.loc in
    let head, head_ty = infer_tm ctx head in

    (* TODO: Proper match compilation *)
    match Core.force_ty head_ty with
    | Variant_type (Row_entries row) ->
        (* Iterate through clauses, accumulating clauses *)
        let clauses =
          List.fold_left
            (fun clauses ({ data = Variant_lit (label, name); _}, body_tm : pattern * _) ->
              if Label_map.mem label.data clauses then
                report ctx
                  (Diagnostic.warning label.loc
                    (Format.asprintf "redundant variant pattern `%s`" label.data));
              match Label_map.find_opt label.data row with
              | Some param_ty ->
                  let body_tm = check_tm (extend ctx name.data param_ty) body_tm body_ty in
                  Label_map.add label.data (name.data, body_tm) clauses
              | None ->
                  error label.loc (Format.asprintf "unexpected variant pattern `%s`" label.data))
            Label_map.empty
            clauses
        in
        (* Check that labels in the clauses match the labels in the row *)
        let missing_clauses =
          Label_map.to_seq row
          |> Seq.filter (fun (label, _) -> not (Label_map.mem label clauses))
          |> List.of_seq
        in
        if List.is_empty missing_clauses then
          (* Return the clauses *)
          Variant_elim (head, clauses)
        else
          error head_loc
            (Format.asprintf "non-exhaustive match, missing %a"
              (Format.pp_print_list
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                (fun ppf (label, _) -> Format.fprintf ppf "`%s`" label))
              missing_clauses)

    | Variant_type (Row_meta_var _) | Meta_var _ as head_ty ->
        (* Build up the clauses and the row from the clauses *)
        let clauses, row =
          List.fold_left
            (fun (clauses, row) ({ data = Variant_lit (label, name); _}, body_tm : pattern * _) ->
              if Label_map.mem label.data clauses then
                report ctx
                  (Diagnostic.warning label.loc
                    (Format.asprintf "redundant variant pattern `%s`" label.data));
              let param_ty = fresh_meta ctx name.loc "pattern binder" in
              let body_tm = check_tm (extend ctx name.data param_ty) body_tm body_ty in
              Label_map.add label.data (name.data, body_tm) clauses,
              Label_map.add label.data param_ty row)
            (Label_map.empty, Label_map.empty)
            clauses
        in
        (* Unify variant type with head type *)
        unify_tys head_loc (Variant_type (Row_entries row)) head_ty;
        (* Return the clauses *)
        Variant_elim (head, clauses)

    | head_ty ->
        error head_loc
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: variant@]@ @[found: %t@]@]"
            (Core.pp_ty head_ty))


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : context -> a) : a option * Diagnostic.t list =
    try
      let ctx = empty () in
      let result = prog ctx in

      ctx.metas |> Dynarray.iter begin fun (loc, info, m) ->
        match !m with
        | Core.Solved _ -> ()
        | Core.Unsolved _ -> report ctx (Diagnostic.error loc ("ambiguous " ^ info))
      end;

      (** Default unsolved rows to fixed row types *)
      ctx.row_metas |> Dynarray.iter begin fun m ->
        match !m with
        | Core.Solved_row _ -> ()
        | Core.Unsolved_row (_, row) ->
            m := Solved_row (Row_entries row);
      end;

      Some result, Dynarray.to_list ctx.diagnostics
    with
    | Error (loc, message) ->
        None, [Diagnostic.error loc message]


  (** {2 Public API} *)

  let check_ty (ty : ty) : Core.ty option * Diagnostic.t list =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : tm) (ty : Core.ty) : Core.tm option * Diagnostic.t list =
    run_elab (fun ctx -> check_tm ctx tm ty)

  let infer_tm (tm : tm) : (Core.tm * Core.ty) option * Diagnostic.t list =
    run_elab (fun ctx -> infer_tm ctx tm)

end
