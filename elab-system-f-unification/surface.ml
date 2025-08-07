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

(** Names that bind definitions or parameters *)
type binder = string option spanned

(** Types in the surface language *)
type ty =
  ty_data spanned

and ty_data =
  | Name of string
  | Placeholder
  | Forall_type of binder list * ty
  | Fun_type of ty * ty

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Int_lit of int
  | App of tm * arg spanned
  | If_then_else of tm * tm * tm
  | Infix of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Prefix of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  | Ty_param of binder
  | Param of binder * ty option

and arg =
  | Ty_arg of ty
  | Arg of tm


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check_ty : ty -> (Core.ty, (span * string) list) result
  val check_tm : tm -> Core.vty -> (Core.tm, (span * string) list) result
  val infer_tm : tm -> (Core.tm * Core.vty, (span * string) list) result

end = struct

  module Semantics = Core.Semantics


  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    ty_size : Core.level;
    ty_names : string option Core.env;
    ty_env : Core.vty Core.env;
    tm_tys : (string option * Core.vty) Core.env;
    metas : (span * string * Core.meta) Dynarray.t;
  }

  (** The empty context *)
  let empty () : context = {
    ty_size = 0;
    ty_names = [];
    ty_env = [];
    tm_tys = [];
    metas = Dynarray.create ();
  }

  (** The type variable that will be bound after calling {!extend_ty} *)
  let next_ty_var (ctx : context) : Core.vty =
    Local_var ctx.ty_size

  (** Extend the context with a type binding *)
  let extend_ty (ctx : context) (name : string option) : context = {
    ctx with
    ty_size = ctx.ty_size + 1;
    ty_names = name :: ctx.ty_names;
    ty_env = next_ty_var ctx :: ctx.ty_env;
  }

  (** Extend the context with a term binding *)
  let extend_tm (ctx : context) (name : string option) (vty : Core.vty) : context = {
    ctx with
    tm_tys = (name, vty) :: ctx.tm_tys;
  }

  (** Lookup a type name in the context *)
  let lookup_ty (ctx : context) (name : string) : Core.index option =
    ctx.ty_names |> List.find_mapi @@ fun ty_index name' ->
      match Some name = name' with
      | true -> Some ty_index
      | false -> None

  (** Lookup a term name in the context *)
  let lookup_tm (ctx : context) (name : string) : (Core.index * Core.vty) option =
    ctx.tm_tys |> List.find_mapi @@ fun tm_index (name', ty) ->
      match Some name = name' with
      | true -> Some (tm_index, ty)
      | false -> None

  (** Generate a fresh metavariable *)
  let fresh_meta (ctx : context) (span: span) (info : string) : Core.ty =
    let m = Core.fresh_meta ctx.ty_size in
    Dynarray.add_last ctx.metas (span, info, m);
    Meta_var m

  let eval_ty (ctx : context) (ty : Core.ty) : Core.vty =
    Semantics.eval_ty ctx.ty_env ty

  let quote_vty (ctx : context) (vty : Core.vty) : Core.ty =
    Semantics.quote_vty ctx.ty_size vty

  let close_vty ({ ty_env; ty_size; _ } : context) (body : Core.vty) : (Core.vty -> Core.vty) =
    fun vty -> Semantics.(eval_ty (vty :: ty_env) (quote_vty (ty_size + 1) body))

  let pp_ty (ctx : context) (ty : Core.ty) : Format.formatter -> unit =
    Core.pp_ty ctx.ty_names ty

  let pp_vty (ctx : context) (vty : Core.vty) : Format.formatter -> unit =
    pp_ty ctx (quote_vty ctx vty)


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

  let unify_vtys (ctx : context) (span : span) (vty1 : Core.vty) (vty2 : Core.vty) =
    try Core.unify_vtys ctx.ty_size vty1 vty2 with
    | Core.Mismatched_types (_, _) ->
        error span
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[found: %t@]@]"
            (pp_vty ctx vty1)
            (pp_vty ctx vty2))
    | Core.Infinite_type m ->
        error span
          (Format.asprintf "@[<v 2>@[meta variable %t refers to itself:@]@ @[expected: %t@]@ @[found: %t@]@]"
            (pp_ty ctx (Meta_var m))
            (pp_vty ctx vty1)
            (pp_vty ctx vty2))
    | Core.Escaping_scope (m, vty) ->
        error span
          (Format.asprintf "@[<v 2>@[type variable %t escapes the scope of meta variable %t:@]@ @[expected: %t@]@ @[found: %t@]@]"
            (pp_vty ctx vty)
            (pp_ty ctx (Meta_var m))
            (pp_vty ctx vty1)
            (pp_vty ctx vty2))


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
    | Name name ->
        begin match lookup_ty ctx name with
        | Some ty_index -> Local_var ty_index
        | None when name = "Bool" -> Bool_type
        | None when name = "Int" -> Int_type
        | None -> error ty.span (Format.asprintf "unbound type `%s`" name)
        end
    | Placeholder ->
        fresh_meta ctx ty.span "placeholder"
    | Forall_type (names, body_ty) ->
        let rec go ctx names : Core.ty =
          match names with
          | [] -> check_ty ctx body_ty
          | name :: names -> Forall_type (name.data, go (extend_ty ctx name.data) names)
        in
        go ctx names
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ctx ty1, check_ty ctx ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (vty : Core.vty) : Core.tm =
    match tm.data, Core.force_vty vty with
    | Let (def_name, params, def_body_ty, def_body, body), vty ->
        let def, def_vty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend_tm ctx def_name.data def_vty) body vty in
        Let (def_name.data, quote_vty ctx def_vty, def, body)

    | Fun_lit (params, body), vty ->
        check_fun_lit ctx params body vty

    | _, Forall_type (name, body_ty) ->
        let name = name |> Option.map (fun n -> "$" ^ n) in
        Forall_lit (name, check_tm (extend_ty ctx name) tm (body_ty (next_ty_var ctx)))

    | If_then_else (head, tm1, tm2), vty ->
        let head = check_tm ctx head Bool_type in
        let tm1 = check_tm ctx tm1 vty in
        let tm2 = check_tm ctx tm2 vty in
        Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _, vty ->
        let tm', vty' = infer_tm ctx tm in
        let tm', vty' = insert ctx tm.span tm' vty' in
        unify_vtys ctx tm.span vty vty';
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.vty =
    match tm.data with
    | Name name ->
        begin match lookup_tm ctx name with
        | Some (tm_index, vty) -> Local_var tm_index, vty
        | None when name = "true" -> Bool_lit true, Bool_type
        | None when name = "false" -> Bool_lit false, Bool_type
        | None -> error tm.span (Format.asprintf "unbound name `%s`" name)
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_vty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend_tm ctx def_name.data def_vty) body in
        Let (def_name.data, quote_vty ctx def_vty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        let vty = eval_ty ctx ty in
        check_tm ctx tm vty, vty

    | Int_lit i ->
        Int_lit i, Int_type

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | App (head, arg) ->
        let head_span = head.span in
        let head, head_ty = infer_tm ctx head in

        let rec go (head : Core.tm) (head_ty : Core.vty) (arg : arg spanned) : Core.tm * Core.vty =
          match Core.force_vty head_ty, arg.data with
          | Forall_type (_, body_ty), Ty_arg arg ->
              let arg = check_ty ctx arg in
              Forall_app (head, arg), body_ty (eval_ty ctx arg)

          (* Instantiate expected type parameters with metavariables *)
          | Forall_type (_, body_ty), Arg _ ->
              let ty_arg = fresh_meta ctx head_span "type argument" in
              go (Forall_app (head, ty_arg)) (body_ty (eval_ty ctx ty_arg)) arg

          | Fun_type (param_ty, body_ty), Arg arg ->
              let arg = check_tm ctx arg param_ty in
              Fun_app (head, arg), body_ty

          | Meta_var _ as head_ty, Arg arg ->
              let param_ty = eval_ty ctx (fresh_meta ctx head_span "function parameter type") in
              let body_ty = eval_ty ctx (fresh_meta ctx head_span "function return type") in
              unify_vtys ctx head_span (Fun_type (param_ty, body_ty)) head_ty;
              let arg = check_tm ctx arg param_ty in
              Fun_app (head, arg), body_ty

          | _, Ty_arg _ -> error arg.span "unexpected type argument"
          | _, Arg _ -> error arg.span "unexpected type argument"
        in

        go head head_ty arg

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let ty = eval_ty ctx (fresh_meta ctx tm.span "if expression branches") in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2), ty

    | Infix (`Eq, tm1, tm2) ->
        let tm1, vty1 = infer_tm ctx tm1 in
        let tm2, vty2 = infer_tm ctx tm2 in
        unify_vtys ctx tm.span vty1 vty2;
        begin match Core.force_vty vty1 with
        | Bool_type -> Prim_app (Bool_eq, [tm1; tm2]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm1; tm2]), Bool_type
        | vty -> error tm.span (Format.asprintf "@[unsupported type: %t@]" (pp_vty ctx vty))
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
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (vty : Core.vty) : Core.tm =
    match params, Core.force_vty vty with
    | [], vty ->
        check_tm ctx body vty

    | Ty_param name :: params, Forall_type (_, body_vty) ->
        let ty_var = next_ty_var ctx in
        let body_tm = check_fun_lit (extend_ty ctx name.data) params body (body_vty ty_var) in
        Forall_lit (name.data, body_tm)

    (* Insert missing type parameters *)
    | params, Forall_type (name, body_vty) ->
        let ty_var = next_ty_var ctx in
        let name = name |> Option.map (fun n -> "$" ^ n) in
        let body_tm = check_fun_lit (extend_ty ctx name) params body (body_vty ty_var) in
        Forall_lit (name, body_tm)

    | Param (name, None) :: params, Fun_type (param_vty, body_vty) ->
        let body_tm = check_fun_lit (extend_tm ctx name.data param_vty) params body body_vty in
        Fun_lit (name.data, quote_vty ctx param_vty, body_tm)

    | Param (name, Some param_ty) :: params, Fun_type (param_vty', body_vty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        let param_vty = eval_ty ctx param_ty in
        unify_vtys ctx param_ty_span param_vty param_vty';
        let body_tm = check_fun_lit (extend_tm ctx name.data param_vty) params body body_vty in
        Fun_lit (name.data, param_ty, body_tm)

    | Param (name, _) :: _, Meta_var _
    | Ty_param name :: _, Meta_var _ ->
        let tm', vty' = infer_fun_lit ctx params None body in
        unify_vtys ctx name.span vty vty';
        tm'

    | Ty_param name :: _, _ ->
        error name.span "unexpected type parameter"

    | Param (name, _) :: _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.vty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        let body_vty = eval_ty ctx body_ty in
        check_tm ctx body body_vty, body_vty

    | [], None ->
        infer_tm ctx body

    | Ty_param name :: params, body_ty ->
        let body, body_ty = infer_fun_lit (extend_ty ctx name.data) params body_ty body in
        Forall_lit (name.data, body), Forall_type (name.data, close_vty ctx body_ty)

    | Param (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta ctx name.span "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let param_vty = eval_ty ctx param_ty in
        let body, body_ty = infer_fun_lit (extend_tm ctx name.data param_vty) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_vty, body_ty)

  and insert (ctx : context) (span : span) (tm : Core.tm) (vty : Core.vty) : Core.tm * Core.vty =
    let rec go (tm : Core.tm) (vty : Core.vty) : Core.tm * Core.vty =
      match Core.force_vty vty with
      | Forall_type (name, body_ty) ->
          let info = Format.asprintf "type argument `%s`" (Option.value name ~default:"_") in
          let arg_vty = fresh_meta ctx span info in
          go (Forall_app (tm, arg_vty)) (body_ty (eval_ty ctx arg_vty))
      | vty -> tm, vty
    in
    match tm with
    | Forall_lit (name, body) -> Forall_lit (name, body), vty
    | tm -> go tm vty



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

  let check_tm (tm : tm) (vty : Core.vty) : (Core.tm, (span * string) list) result =
    run_elab (fun ctx -> check_tm ctx tm vty)

  let infer_tm (tm : tm) : (Core.tm * Core.vty, (span * string) list) result =
    run_elab (fun ctx -> infer_tm ctx tm)

end
