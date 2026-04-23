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
  | Prim of string
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

  val check_ty : ty -> (Core.Ty.t, (span * string) list) result
  val check_tm : tm -> Core.Ty.value -> (Core.Tm.t, (span * string) list) result
  val infer_tm : tm -> (Core.Tm.t * Core.Ty.value, (span * string) list) result

end = struct

  (** {2 Elaboration context} *)

  (** The elaboration context *)
  type context = {
    ty_size : Core.level;
    ty_names : string option Core.env;
    ty_env : Core.Ty.value Core.env;
    tm_tys : (string option * Core.Ty.value) Core.env;
    metas : (span * string * Core.Ty.meta) Dynarray.t;
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
  let next_ty_var (ctx : context) : Core.Ty.value =
    Core.Ty.next_var ctx.ty_size

  (** Extend the context with a type binding *)
  let extend_ty (ctx : context) (name : string option) : context = {
    ctx with
    ty_size = ctx.ty_size + 1;
    ty_names = name :: ctx.ty_names;
    ty_env = (next_ty_var ctx) :: ctx.ty_env;
  }

  (** Extend the context with a term binding *)
  let extend_tm (ctx : context) (name : string option) (vty : Core.Ty.value) : context = {
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
  let lookup_tm (ctx : context) (name : string) : (Core.index * Core.Ty.value) option =
    ctx.tm_tys |> List.find_mapi @@ fun tm_index (name', ty) ->
      match Some name = name' with
      | true -> Some (tm_index, ty)
      | false -> None

  (** Generate a fresh metavariable *)
  let fresh_meta (ctx : context) (span: span) (info : string) : Core.Ty.t =
    let m = Core.Ty.fresh_meta ctx.ty_size in
    Dynarray.add_last ctx.metas (span, info, m);
    Core.Ty.Meta_var m

  let eval_ty (ctx : context) (ty : Core.Ty.t) : Core.Ty.value =
    Core.Ty.eval ctx.ty_env ty

  let quote_vty (ctx : context) (vty : Core.Ty.value) : Core.Ty.t =
    Core.Ty.quote ctx.ty_size vty

  let close_vty (ctx : context) (body : Core.Ty.value) : Core.Ty.clos =
    Core.Ty.(Clos (ctx.ty_env, quote (ctx.ty_size + 1) body))

  let pp_ty (ctx : context) (ty : Core.Ty.t) : Format.formatter -> unit =
    Core.Ty.pp ctx.ty_names ty

  let pp_vty (ctx : context) (vty : Core.Ty.value) : Format.formatter -> unit =
    pp_ty ctx (quote_vty ctx vty)


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of (span * string)

  (** Raise an elaboration error with a formatted message *)
  let error (type a b) (span : span) : (b, Format.formatter, unit, a) format4 -> b =
    Format.kasprintf (fun message -> raise (Error (span, message)))

  let unify_vtys (ctx : context) (span : span) ~(found : Core.Ty.value) ~(expected : Core.Ty.value) =
    try Core.Ty.unify ctx.ty_size found expected with
    | Core.Ty.Mismatched_types (_, _) ->
        error span "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (pp_vty ctx expected)
          (pp_vty ctx found)
    | Core.Ty.Infinite_type m ->
        error span "@[<v 2>@[meta variable %t refers to itself:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (pp_ty ctx (Meta_var m))
          (pp_vty ctx expected)
          (pp_vty ctx found)
    | Core.Ty.Escaping_scope (m, vty) ->
        error span "@[<v 2>@[type variable %t escapes the scope of meta variable %t:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (pp_vty ctx vty)
          (pp_ty ctx (Meta_var m))
          (pp_vty ctx expected)
          (pp_vty ctx found)


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ctx : context) (ty : ty) : Core.Ty.t =
    match ty.data with
    | Name name ->
        begin match lookup_ty ctx name with
        | Some ty_index -> Local_var ty_index
        | None when name = "Bool" -> Core.Ty.Bool
        | None when name = "Int" -> Core.Ty.Int
        | None -> error ty.span "unbound type `%s`" name
        end
    | Placeholder ->
        fresh_meta ctx ty.span "placeholder"
    | Forall_type (names, body_ty) ->
        let rec go ctx names : Core.Ty.t =
          match names with
          | [] -> check_ty ctx body_ty
          | name :: names ->
              Core.Ty.Forall (name.data, go (extend_ty ctx name.data) names)
        in
        go ctx names
    | Fun_type (ty1, ty2) ->
        Core.Ty.Fun (check_ty ctx ty1, check_ty ctx ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (vty : Core.Ty.value) : Core.Tm.t =
    match tm.data, Core.Ty.force vty with
    | Let (def_name, params, def_body_ty, def_body, body), vty ->
        let def, def_vty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm (extend_tm ctx def_name.data def_vty) body vty in
        Core.Tm.Let (def_name.data, quote_vty ctx def_vty, def, body)

    | Fun_lit (params, body), vty ->
        check_fun_lit ctx tm.span params body vty

    | _, Core.Ty.Forall (name, body_ty) ->
        let name = name |> Option.map (fun n -> "$" ^ n) in
        let body_vty = Core.Ty.inst_clos body_ty (next_ty_var ctx) in
        Core.Tm.Forall_lit (name, check_tm (extend_ty ctx name) tm body_vty)

    | If_then_else (head, tm1, tm2), vty ->
        let head = check_tm ctx head Core.Ty.Bool in
        let tm1 = check_tm ctx tm1 vty in
        let tm2 = check_tm ctx tm2 vty in
        Core.Tm.Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _, vty ->
        let tm', vty' = infer_tm ctx tm in
        let tm', vty' = insert ctx tm.span tm' vty' in
        unify_vtys ctx tm.span ~found:vty' ~expected:vty;
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.Tm.t * Core.Ty.value =
    match tm.data with
    | Name name ->
        begin match lookup_tm ctx name with
        | Some (tm_index, vty) -> Core.Tm.Local_var tm_index, vty
        | None when name = "true" -> Core.Tm.Bool_lit true, Core.Ty.Bool
        | None when name = "false" -> Core.Tm.Bool_lit false, Core.Ty.Bool
        | None -> error tm.span "unbound name `%s`" name
        end

    | Prim name ->
        begin match Prim.of_name name with
        | Some (Bool_eq as prim) -> Core.(Tm.Prim prim, Ty.(Fun (Bool, Fun (Bool, Bool))))
        | Some (Int_eq as prim) -> Core.(Tm.Prim prim, Ty.(Fun (Int, Fun (Int, Bool))))
        | Some (Int_add as prim) -> Core.(Tm.Prim prim, Ty.(Fun (Int, Fun (Int, Int))))
        | Some (Int_sub as prim) -> Core.(Tm.Prim prim, Ty.(Fun (Int, Fun (Int, Int))))
        | Some (Int_mul as prim) -> Core.(Tm.Prim prim, Ty.(Fun (Int, Fun (Int, Int))))
        | Some (Int_neg as prim) -> Core.(Tm.Prim prim, Ty.(Fun (Int, Int)))
        | None -> error tm.span "unknown primitive operation `#%s`" name
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_vty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm (extend_tm ctx def_name.data def_vty) body in
        Core.Tm.Let (def_name.data, quote_vty ctx def_vty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        let vty = eval_ty ctx ty in
        check_tm ctx tm vty, vty

    | Int_lit i ->
        Core.Tm.Int_lit i, Core.Ty.Int

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | App (head, arg) ->
        let head_span = head.span in
        let head, head_ty = infer_tm ctx head in

        let rec go (head : Core.Tm.t) (head_ty : Core.Ty.value) (arg : arg spanned) : Core.Tm.t * Core.Ty.value =
          match Core.Ty.force head_ty, arg.data with
          | Core.Ty.Forall (_, body_ty), Ty_arg arg ->
              let arg = check_ty ctx arg in
              Core.Tm.Forall_app (head, arg), Core.Ty.inst_clos body_ty (eval_ty ctx arg)

          (* Instantiate expected type parameters with metavariables *)
          | Core.Ty.Forall (_, body_ty), Arg _ ->
              let ty_arg = fresh_meta ctx head_span "type argument" in
              go (Core.Tm.Forall_app (head, ty_arg)) (Core.Ty.inst_clos body_ty (eval_ty ctx ty_arg)) arg

          | Core.Ty.Fun (param_ty, body_ty), Arg arg ->
              let arg = check_tm ctx arg param_ty in
              Core.Tm.Fun_app (head, arg), body_ty

          | Core.Ty.Meta_var _ as head_ty, Arg arg ->
              let arg_ty = eval_ty ctx (fresh_meta ctx head_span "function argument") in
              let body_ty = eval_ty ctx (fresh_meta ctx head_span "function return type") in
              unify_vtys ctx head_span ~expected:head_ty ~found:(Core.Ty.Fun (arg_ty, body_ty));
              let arg = check_tm ctx arg arg_ty in
              Core.Tm.Fun_app (head, arg), body_ty

          | _, Ty_arg _ -> error arg.span "unexpected type argument"
          | _, Arg _ -> error arg.span "unexpected type argument"
        in

        go head head_ty arg

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Core.Ty.Bool in
        let ty = eval_ty ctx (fresh_meta ctx tm.span "if expression branches") in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Core.Tm.Bool_elim (head, tm1, tm2), ty

    | Infix (`Eq, tm1, tm2) ->
        let tm1, vty1 = infer_tm ctx tm1 in
        let tm2, vty2 = infer_tm ctx tm2 in
        unify_vtys ctx tm.span ~found:vty2 ~expected:vty1;
        begin match Core.Ty.force vty1 with
        | Core.Ty.Bool -> Core.(Tm.Fun_app (Fun_app (Prim Prim.Bool_eq, tm1), tm2), Ty.Bool)
        | Core.Ty.Int -> Core.(Tm.Fun_app (Fun_app (Prim Prim.Int_eq, tm1), tm2), Ty.Bool)
        | vty -> error tm.span "@[unsupported type: %t@]" (pp_vty ctx vty)
        end

    | Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Core.Ty.Int in
        let tm2 = check_tm ctx tm2 Core.Ty.Int in
        Core.Tm.Fun_app (Fun_app (Prim prim, tm1), tm2), Core.Ty.Int

    | Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Core.Ty.Int in
        Core.Tm.Fun_app (Prim Prim.Int_neg, tm), Core.Ty.Int

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (span : span) (params : param list) (body : tm) (vty : Core.Ty.value) : Core.Tm.t =
    match params, Core.Ty.force vty with
    | [], vty ->
        check_tm ctx body vty

    | Ty_param name :: params, Core.Ty.Forall (_, body_vty) ->
        let body_vty = Core.Ty.inst_clos body_vty (next_ty_var ctx) in
        let body_tm = check_fun_lit (extend_ty ctx name.data) span params body body_vty in
        Core.Tm.Forall_lit (name.data, body_tm)

    (* Insert missing type parameters *)
    | params, Core.Ty.Forall (name, body_vty) ->
        let name = name |> Option.map (fun n -> "$" ^ n) in
        let body_vty = Core.Ty.inst_clos body_vty (next_ty_var ctx) in
        let body_tm = check_fun_lit (extend_ty ctx name) span params body body_vty in
        Core.Tm.Forall_lit (name, body_tm)

    | Param (name, None) :: params, Core.Ty.Fun (param_vty, body_vty) ->
        let body_tm = check_fun_lit (extend_tm ctx name.data param_vty) span params body body_vty in
        Core.Tm.Fun_lit (name.data, quote_vty ctx param_vty, body_tm)

    | Param (name, Some param_ty) :: params, Core.Ty.Fun (param_vty', body_vty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        let param_vty = eval_ty ctx param_ty in
        unify_vtys ctx param_ty_span ~found:param_vty ~expected:param_vty';
        let body_tm = check_fun_lit (extend_tm ctx name.data param_vty) span params body body_vty in
        Core.Tm.Fun_lit (name.data, param_ty, body_tm)

    | (_ :: _) as params, Core.Ty.Meta_var _ ->
        let tm', vty' = infer_fun_lit ctx params None body in
        unify_vtys ctx span ~found:vty' ~expected:vty;
        tm'

    | Ty_param name :: _, _ ->
        error name.span "unexpected type parameter"

    | Param (name, _) :: _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.Tm.t * Core.Ty.value =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        let body_vty = eval_ty ctx body_ty in
        check_tm ctx body body_vty, body_vty

    | [], None ->
        infer_tm ctx body

    | Ty_param name :: params, body_ty ->
        let body, body_ty = infer_fun_lit (extend_ty ctx name.data) params body_ty body in
        Core.Tm.Forall_lit (name.data, body), Core.Ty.Forall (name.data, close_vty ctx body_ty)

    | Param (name, param_ty) :: params, body_ty ->
        let param_ty = match param_ty with
          | None -> fresh_meta ctx name.span "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let param_vty = eval_ty ctx param_ty in
        let body, body_ty = infer_fun_lit (extend_tm ctx name.data param_vty) params body_ty body in
        Core.Tm.Fun_lit (name.data, param_ty, body), Core.Ty.Fun (param_vty, body_ty)

  and insert (ctx : context) (span : span) (tm : Core.Tm.t) (vty : Core.Ty.value) : Core.Tm.t * Core.Ty.value =
    let rec go (tm : Core.Tm.t) (vty : Core.Ty.value) : Core.Tm.t * Core.Ty.value =
      match Core.Ty.force vty with
      | Core.Ty.Forall (name, body_ty) ->
          let info = Format.asprintf "type argument `%s`" (Option.value name ~default:"_") in
          let arg_vty = fresh_meta ctx span info in
          let body_vty = Core.Ty.inst_clos body_ty (eval_ty ctx arg_vty) in
          go (Forall_app (tm, arg_vty)) body_vty
      | vty -> tm, vty
    in
    match tm with
    | Forall_lit (name, body) -> Forall_lit (name, body), vty
    | tm -> go tm vty



  (** {2 Running elaboration} *)

  let collect_ambiguities (ctx : context) : (span * string) list =
    let ambiguity_error (span, info, m) =
      match !m with
      | Core.Ty.Unsolved _ -> Some (span, "ambiguous " ^ info)
      | Core.Ty.Solved _ -> None
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

  let check_ty (ty : ty) : (Core.Ty.t, (span * string) list) result =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : tm) (vty : Core.Ty.value) : (Core.Tm.t, (span * string) list) result =
    run_elab (fun ctx -> check_tm ctx tm vty)

  let infer_tm (tm : tm) : (Core.Tm.t * Core.Ty.value, (span * string) list) result =
    run_elab (fun ctx -> infer_tm ctx tm)

end
