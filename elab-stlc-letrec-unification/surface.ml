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

(** Types in the surface language *)
type ty =
  ty_data spanned

and ty_data =
  | Name of string
  | Fun_type of ty * ty
  | Placeholder

(** Names that bind definitions or parameters *)
type binder = string option spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Let of defn * tm
  | Let_rec of defn list * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Int_lit of int
  | App of tm * tm
  | If_then_else of tm * tm * tm
  | Infix of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Prefix of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option

(** Definitions *)
and defn =
  binder * param list * ty option * tm


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check_ty : ty -> (Core.ty, (span * string) list) result
  val check_tm : tm -> Core.ty -> (Core.tm, (span * string) list) result
  val infer_tm : tm -> (Core.tm * Core.ty, (span * string) list) result

end = struct

  module Label_map = Core.Label_map

  (** {2 Elaboration context} *)

  (** An entry in the context *)
  type entry =
    | Param of string option * Core.ty
    | Fields of Core.ty Label_map.t

  (** The elaboration context *)
  type context = {
    tys : entry Core.env;
    (** A stack of bindings currently in scope *)

    metas : (span * string * Core.meta) Dynarray.t;
    (** A list of the metavariables that have been inserted during elaboration.
        This will be used to generate a list of unsolved metavariables once
        elaboration is complete. *)
  }

  (** The empty context *)
  let empty () : context = {
    tys = [];
    metas = Dynarray.create ();
  }

  (** Extend the context with a new binding *)
  let extend (ctx : context) (entry : entry) : context = {
    ctx with
    tys = entry :: ctx.tys;
  }

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.tm * Core.ty) option =
    ctx.tys |> List.find_mapi @@ fun index entry ->
      match entry with
      | Param (name', ty) when Some name = name' ->
          Some (Core.Var index, ty)
      | Param (_, _) -> None
      | Fields defs ->
          Label_map.find_opt name defs |> Option.map @@ fun ty ->
            Core.Record_proj (Var index, name), ty

  (** Generate a fresh metavariable *)
  let fresh_meta (ctx : context) (span: span) (info : string) : Core.ty =
    let m = Core.fresh_meta () in
    Dynarray.add_last ctx.metas (span, info, m);
    Core.Meta_var m


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of (span * string)

  (** Raises an {!Error} exception *)
  let error (type a) (span : span) (message : string) : a =
    raise (Error (span, message))

  let unify_tys (span : span) ~(found : Core.ty) ~(expected : Core.ty) =
    try Core.unify_tys found expected with
    | Core.Infinite_type _ -> error span "infinite type"
    | Core.Mismatched_types (_, _) ->
        error span
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
            (Core.pp_ty expected)
            (Core.pp_ty found))


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
    | Name name ->
        error ty.span (Format.asprintf "unbound type `%s`" name)
    | Fun_type (ty1, ty2) ->
        Core.Fun_type (check_ty ctx ty1, check_ty ctx ty2)
    | Placeholder ->
        fresh_meta ctx ty.span "placeholder"

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let ((def_name, params, def_ty, def), body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let body = check_tm (extend ctx (Param (def_name.data, def_ty))) body ty in
        Core.Let (def_name.data, def_ty, def, body)

    | Let_rec (defs, body) ->
        let def_name, defs_entry, def_ty, def = elab_let_rec_defs ctx defs in
        let body = check_tm (extend ctx defs_entry) body ty in
        Core.Let (def_name, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params None body ty

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Core.Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Core.Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        unify_tys tm.span ~found:ty' ~expected:ty;
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (tm, ty) -> tm, ty
        | None when name = "true" -> Core.Bool_lit true, Core.Bool_type
        | None when name = "false" -> Core.Bool_lit false, Core.Bool_type
        | None -> error tm.span (Format.asprintf "unbound name `%s`" name)
        end

    | Let ((def_name, params, def_ty, def), body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let body, body_ty = infer_tm (extend ctx (Param (def_name.data, def_ty))) body in
        Core.Let (def_name.data, def_ty, def, body), body_ty

    | Let_rec (defs, body) ->
        let def_name, defs_entry, def_ty, def = elab_let_rec_defs ctx defs in
        let body, body_ty = infer_tm (extend ctx defs_entry) body in
        Core.Let (def_name, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        check_tm ctx tm ty, ty

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

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
            unify_tys head_span ~found:head_ty ~expected:(Core.Fun_type (param_ty, body_ty));
            let arg = check_tm ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty
        | _ -> error arg.span "unexpected argument"
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
        unify_tys tm.span ~found:ty2 ~expected:ty1;
        begin match Core.force_ty ty1 with
        | Core.Bool_type -> Core.Prim_app (Prim.Bool_eq, [tm1; tm2]), Core.Bool_type
        | Core.Int_type -> Core.Prim_app (Prim.Int_eq, [tm1; tm2]), Core.Bool_type
        | ty -> error tm.span (Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty))
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
  and check_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) (ty : Core.ty) : Core.tm =
    match params, body_ty, Core.force_ty ty with
    | [], None, ty -> check_tm ctx body ty
    | [], Some ({ span; _ } as body_ty), ty ->
        let body_ty = check_ty ctx body_ty in
        unify_tys span ~found:body_ty ~expected:ty;
        check_tm ctx body body_ty
    | (name, None) :: params, body_ty, Core.Fun_type (param_ty, ty) ->
        let body = check_fun_lit (extend ctx (Param (name.data, param_ty))) params body_ty body ty in
        Core.Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, body_ty, Core.Fun_type (param_ty', ty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        unify_tys param_ty_span ~found:param_ty ~expected:param_ty';
        let body = check_fun_lit (extend ctx (Param (name.data, param_ty))) params body_ty body ty in
        Core.Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, body_ty, Core.Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params body_ty body in
        unify_tys name.span ~found:ty' ~expected:ty;
        tm'
    | (name, _) :: _, _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function literal, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, None) :: params, body_ty ->
        let param_ty = fresh_meta ctx name.span "function parameter type" in
        let body, body_ty = infer_fun_lit (extend ctx (Param (name.data, param_ty))) params body_ty body in
        Core.Fun_lit (name.data, param_ty, body), Core.Fun_type (param_ty, body_ty)
    | (name, Some param_ty) :: params, body_ty ->
        let param_ty = check_ty ctx param_ty in
        let body, body_ty = infer_fun_lit (extend ctx (Param (name.data, param_ty))) params body_ty body in
        Core.Fun_lit (name.data, param_ty, body), Core.Fun_type (param_ty, body_ty)

  (** Elaborate the definitions of a recursive let binding. *)
  and elab_let_rec_defs (ctx : context) (defs : defn list) : string option * entry * Core.ty * Core.tm =
    (* Check a definition, avoiding “vicious circles” *)
    let check_def ctx def_name params body_ty def def_ty =
      match check_fun_lit ctx params body_ty def def_ty with
      (* Avoid “vicious circles” with a blunt syntactic check on the elaborated
        term, ensuring that it is a function literal. This is similar to the
        approach used in Standard ML. This rules out definitions like:

            let x := x + 1;

        OCaml uses a more complicated check (as of versions 4.06 and 4.08) that
        admits more valid programs. A detailed description of this check can be
        found in “A practical mode system for recursive definitions” by Reynaud
        et. al. https://doi.org/10.1145/3434326. *)
      | Core.Fun_lit _ as def -> def
      | _ -> error def_name.span "expected function literal in recursive let binding"
    in

    (* Elaborate the recursive definitions, special-casing singly recursive
       definitions. *)
    match defs with
    (* Singly recursive definitions are special-cased. This is not exactly
       necessary, but avoids the need to construct an intermediate record.*)
    | [(def_name, params, body_ty, def)] ->
        let def_ty = fresh_meta ctx def_name.span "definition type" in
        let def_entry = Param (def_name.data, def_ty) in
        let def = check_def (extend ctx def_entry) def_name params body_ty def def_ty in

        def_name.data, def_entry, def_ty, Core.Fix (def_name.data, def_ty, def)

    (* Mutually recursive definitions *)
    | defs ->
        let mutual_decls =
          defs |> ListLabels.fold_left
            ~init:Label_map.empty
            ~f:(fun mutual_decls (def_name, _, _, _) ->
              match def_name.data with
              | Some name when not (Label_map.mem name mutual_decls) ->
                  mutual_decls |> Label_map.add name (fresh_meta ctx def_name.span "definition type")
              | Some name -> error def_name.span (Format.asprintf "duplicate name `%s` in mutually recursive bindings" name)
              | None -> error def_name.span (Format.asprintf "cannot use `_` in mutually recursive bindings"))
        in

        let mutual_defs =
          let ctx = extend ctx (Fields mutual_decls) in
          defs |> ListLabels.fold_left
            ~init:Label_map.empty
            ~f:(fun mutual_defs (def_name, params, body_ty, def) ->
              match def_name.data with
              | Some name ->
                  let def_ty = Label_map.find name mutual_decls in
                  let def = check_def ctx def_name params body_ty def def_ty in
                  mutual_defs |> Label_map.add name def
              | None ->
                  mutual_defs)
        in

        let def_name = Format.sprintf "$mutual-%i" (List.length ctx.tys) in

        Some def_name,
        Fields mutual_decls,
        Core.Record_type mutual_decls,
        Core.Fix (Some def_name, Core.Record_type mutual_decls, Core.Record_lit mutual_defs)


  (** {2 Running elaboration} *)

  let collect_ambiguities (ctx : context) : (span * string) list =
    let ambiguity_error (span, info, m) =
      match !m with
      | Core.Unsolved _ -> Some (span, "ambiguous " ^ info)
      | Core.Solved _ -> None
    in
    Dynarray.to_seq ctx.metas
    |> Seq.filter_map ambiguity_error
    |> List.of_seq

  let run_elab (type a) (prog : context -> a) : (a, (span * string) list) result =
    match
      let ctx = empty () in
      let result = prog ctx in
      let ambiguity_errors = collect_ambiguities ctx in
      result, ambiguity_errors
    with
    | result, [] -> Ok result
    | _, errors -> Error errors
    | exception Error (span, message) -> Error [(span, message)]


  (** {2 Public API} *)

  let check_ty (ty : ty) : (Core.ty, (span * string) list) result =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : tm) (ty : Core.ty) : (Core.tm, (span * string) list) result =
    run_elab (fun ctx -> check_tm ctx tm ty)

  let infer_tm (tm : tm) : (Core.tm * Core.ty, (span * string) list) result =
    run_elab (fun ctx -> infer_tm ctx tm)

end
