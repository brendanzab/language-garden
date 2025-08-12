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
  | Bool_lit of bool
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

  (** {2 Elaboration context} *)

  (** An entry in the context *)
  type entry =
    | Def of string option * Core.ty
    | Mutual_def of (string option * Core.ty) list

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
      | Def (name', ty) when Some name = name' ->
          Some (Core.Var index, ty)
      | Def (_, _) -> None
      | Mutual_def entries ->
          entries |> List.find_mapi @@ fun elem_index (name', ty) ->
            match Some name = name' with
            | true -> Some (Core.Tuple_proj (Var index, elem_index), ty)
            | false -> None

  (** Generate a fresh metavariable *)
  let fresh_meta (ctx : context) (span: span) (info : string) : Core.ty =
    let m = Core.fresh_meta () in
    Dynarray.add_last ctx.metas (span, info, m);
    Meta_var m


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
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[found: %t@]@]"
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
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name ->
        error ty.span (Format.asprintf "unbound type `%s`" name)
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ctx ty1, check_ty ctx ty2)
    | Placeholder ->
        fresh_meta ctx ty.span "placeholder"

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let ((def_name, params, def_ty, def), body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let body = check_tm (extend ctx (Def (def_name.data, def_ty))) body ty in
        Let (def_name.data, def_ty, def, body)

    | Let_rec (defns, body) ->
        let def_name, defs_entry, def_ty, def = elab_let_rec_defns ctx defns in
        let body = check_tm (extend ctx defs_entry) body ty in
        Let (def_name, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body ty

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2)

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
        | None when name = "true" -> Bool_lit true, Bool_type
        | None when name = "false" -> Bool_lit false, Bool_type
        | None -> error tm.span (Format.asprintf "unbound name `%s`" name)
        end

    | Let ((def_name, params, def_ty, def), body) ->
        let def, def_ty = infer_fun_lit ctx params def_ty def in
        let body, body_ty = infer_tm (extend ctx (Def (def_name.data, def_ty))) body in
        Let (def_name.data, def_ty, def, body), body_ty

    | Let_rec (defns, body) ->
        let def_name, defs_entry, def_ty, def = elab_let_rec_defns ctx defns in
        let body, body_ty = infer_tm (extend ctx defs_entry) body in
        Let (def_name, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        check_tm ctx tm ty, ty

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | Int_lit i ->
        Int_lit i, Int_type

    | Bool_lit b ->
        Bool_lit b, Bool_type

    | App ({ span = head_span; _ } as head, arg) ->
        let head, head_ty = infer_tm ctx head in
        begin match Core.force_ty head_ty with
        | Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Fun_app (head, arg), body_ty
        | Meta_var _ as head_ty ->
            let param_ty = fresh_meta ctx head_span "function parameter type" in
            let body_ty = fresh_meta ctx head_span "function return type" in
            unify_tys head_span ~found:head_ty ~expected:(Fun_type (param_ty, body_ty));
            let arg = check_tm ctx arg param_ty in
            Fun_app (head, arg), body_ty
        | _ -> error arg.span "unexpected argument"
        end

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let ty = fresh_meta ctx tm.span "if expression branches" in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2), ty

    | Infix (`Eq, tm1, tm2) ->
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        unify_tys tm.span ~found:ty2 ~expected:ty1;
        begin match Core.force_ty ty1 with
        | Bool_type -> Prim_app (Bool_eq, [tm1; tm2]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm1; tm2]), Bool_type
        | ty -> error tm.span (Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty))
        end

    | Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Int_type in
        let tm2 = check_tm ctx tm2 Int_type in
        Prim_app (prim, [tm1; tm2]), Int_type

    | Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Int_type in
        Prim_app (Int_neg, [tm]), Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    match params, Core.force_ty ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit (extend ctx (Def (name.data, param_ty))) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        unify_tys param_ty_span ~found:param_ty ~expected:param_ty';
        let body = check_fun_lit (extend ctx (Def (name.data, param_ty))) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params None body in
        unify_tys name.span ~found:ty' ~expected:ty;
        tm'
    | (name, _) :: _, _ ->
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
        let body, body_ty = infer_fun_lit (extend ctx (Def (name.data, param_ty))) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)
    | (name, Some param_ty) :: params, body_ty ->
        let param_ty = check_ty ctx param_ty in
        let body, body_ty = infer_fun_lit (extend ctx (Def (name.data, param_ty))) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)

  (** Elaborate the definitions of a recursive let binding. *)
  and elab_let_rec_defns (ctx : context) (defns : defn list) : string option * entry * Core.ty * Core.tm =
    (* Creates a fresh function type for a definition. *)
    let rec fresh_fun_ty ctx span params body_ty =
      match params, body_ty with
      | [], Some body_ty -> check_ty ctx body_ty
      | [], None -> fresh_meta ctx span "function return type"
      | (name, _) :: params, body_ty ->
          let param_ty = fresh_meta ctx name.span "function parameter type" in
          Fun_type (param_ty, fresh_fun_ty (extend ctx (Def (name.data, param_ty))) span params body_ty)
    in

    (* Check the body of a definition, ensuring it is a function *)
    let check_def_body ctx params def_span def def_ty =
      (* Avoid “vicious circles” with a blunt syntactic check on the elaborated
        term, ensuring that it is a function literal. This is similar to the
        approach used in Standard ML. This rules out definitions like:

            let x := x + 1;

        OCaml uses a more complicated check (as of versions 4.06 and 4.08) that
        admits more valid programs. A detailed description of this check can be
        found in “A practical mode system for recursive definitions” by Reynaud
        et. al. https://doi.org/10.1145/3434326. *)
      match check_fun_lit ctx params def def_ty with
      | Fun_lit _ as def_body -> def_body
      | _ -> error def_span "expected function literal in recursive let binding"
    in

    (* Check a series of mutually recursive definitions against their types *)
    let check_def_bodies ctx =
      List.map2 (fun (def_name, params, _, def) (_, def_ty) ->
        check_def_body ctx params def_name.span def def_ty)
    in

    (* Elaborate the recursive definitions, special-casing singly recursive
      definitions. The special-casing is not exactly necessary, but does reduce
      indirections during evaluation. *)
    match defns with
    (* Singly recursive definitions *)
    | [(def_name, params, def_ty, def)] ->
        let def_ty = fresh_fun_ty ctx def_name.span params def_ty in
        let def_entry = Def (def_name.data, def_ty) in
        let def_body = check_def_body (extend ctx def_entry) params def_name.span def def_ty in
        let def = Core.Fix (def_name.data, def_ty, def_body) in

        def_name.data, def_entry, def_ty, def

    (* Mutually recursive definitions *)
    | defs ->
        let def_tys = defs |> List.map (fun (def_name, params, def_ty, _) ->
          def_name.data, fresh_fun_ty ctx def_name.span params def_ty)
        in
        let defs_entry = Mutual_def def_tys in
        let defs = check_def_bodies (extend ctx defs_entry) defs def_tys in

        (* Create the combined mutual definition using a tuple and the fixed-point
          combinator. Alternatively we could use a record here, which could make
          debugging the elaborated terms a little easier. *)
        let def_name = Printf.sprintf "$mutual-%i" (List.length ctx.tys) in
        let def_ty = Core.Tuple_type (List.map snd def_tys) in
        let def = Core.Fix (Some def_name, def_ty, Tuple_lit defs) in

        Some def_name, defs_entry, def_ty, def


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
