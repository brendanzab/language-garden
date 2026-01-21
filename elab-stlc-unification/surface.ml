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
  | Prim of string
  | Let of binder * param list * ty option * tm * tm
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

  (** The elaboration context *)
  type context = {
    tys : (Core.name * Core.ty) Core.env;
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
  let extend (ctx : context) (name : Core.name) (ty : Core.ty) : context = {
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


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of span * string

  (** Raise an elaboration error with a formatted message *)
  let error (type a b) (span : span) : (b, Format.formatter, unit, a) format4 -> b =
    Format.kasprintf (fun message -> raise (Error (span, message)))

  let unify_tys (span : span) ~(found : Core.ty) ~(expected : Core.ty) =
    try Core.unify_tys found expected with
    | Core.Infinite_type _ -> error span "infinite type"
    | Core.Mismatched_types (_, _) ->
        error span "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (Core.pp_ty expected)
          (Core.pp_ty found)


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
    | Name name -> error ty.span "unbound type `%s`" name
    | Fun_type (ty1, ty2) ->
        Core.Fun_type (check_ty ctx ty1, check_ty ctx ty2)
    | Placeholder ->
        fresh_meta ctx ty.span "placeholder"

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend ctx def_name.data def_ty) body ty in
        Core.Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx tm.span params body ty

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
        | Some (index, ty) -> Core.Var index, ty
        | None when name = "true" -> Core.Bool_lit true, Core.Bool_type
        | None when name = "false" -> Core.Bool_lit false, Core.Bool_type
        | None -> error tm.span "unbound name `%s`" name
        end

    | Prim name ->
        begin match Prim.of_name name with
        | Some (Bool_eq as prim) -> Core.Prim prim, Core.(Fun_type (Bool_type, Fun_type (Bool_type, Bool_type)))
        | Some (Int_eq as prim) -> Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Bool_type)))
        | Some (Int_add as prim) -> Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Int_type)))
        | Some (Int_sub as prim) -> Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Int_type)))
        | Some (Int_mul as prim) -> Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Int_type)))
        | Some (Int_neg as prim) -> Core.Prim prim, Core.(Fun_type (Int_type, Int_type))
        | None -> error tm.span "unknown primitive operation `#%s`" name
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

    | Int_lit i ->
        Core.Int_lit i, Core.Int_type

    | App ({ span = head_span; _ } as head, arg) ->
        let head, head_ty = infer_tm ctx head in
        begin match Core.force_ty head_ty with
        | Core.Fun_type (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Core.Fun_app (head, arg), body_ty
        | Meta_var _ as head_ty ->
            let arg_ty = fresh_meta ctx arg.span "function argument" in
            let body_ty = fresh_meta ctx head_span "function return type" in
            unify_tys head_span ~found:head_ty ~expected:(Core.Fun_type (arg_ty, body_ty));
            let arg = check_tm ctx arg arg_ty in
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
        | Core.Bool_type -> Core.(fun_app (Prim Prim.Bool_eq) [tm1; tm2]), Core.Bool_type
        | Core.Int_type -> Core.(fun_app (Prim Prim.Int_eq) [tm1; tm2]), Core.Bool_type
        | ty -> error tm.span "@[unsupported type: %t@]" (Core.pp_ty ty)
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
        Core.(fun_app (Prim prim) [tm1; tm2]), Core.Int_type

    | Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Core.Int_type in
        Core.(fun_app (Prim Prim.Int_neg) [tm]), Core.Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (span : span) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    match params, Core.force_ty ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Core.Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit (extend ctx name.data param_ty) span params body body_ty in
        Core.Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Core.Fun_type (param_ty', body_ty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        unify_tys param_ty_span ~found:param_ty ~expected:param_ty';
        let body = check_fun_lit (extend ctx name.data param_ty) span params body body_ty in
        Core.Fun_lit (name.data, param_ty, body)
    | (_ :: _) as params, Core.Meta_var _ ->
        let tm', ty' = infer_fun_lit ctx params None body in
        unify_tys span ~found:ty' ~expected:ty;
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
    | (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta ctx name.span "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let body, body_ty = infer_fun_lit (extend ctx name.data param_ty) params body_ty body in
        Core.Fun_lit (name.data, param_ty, body), Core.Fun_type (param_ty, body_ty)


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
