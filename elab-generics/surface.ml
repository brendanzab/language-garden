(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
module Span = struct

  type t =
    Lexing.position * Lexing.position

end

(** Spanned nodes *)
module Spanned = struct

  type 'a t = {
    span : Span.t;
    data : 'a;
  }

end

(** Types in the surface language *)
module Ty = struct

  type t =
    data Spanned.t

  and data =
    | Name of string
    | Fun of t * t
    | Tuple of t list
    | Placeholder

  (** Names that bind type definitions or parameters *)
  and binder =
    string Spanned.t

end

(** Terms in the surface language *)
module Tm = struct

  type t =
    data Spanned.t

  and data =
    | Name of string Spanned.t * Ty.t list
    | Let of [`Rec] option * def list * t
    | Ann of t * Ty.t
    | Fun of fun_
    | Tuple of t list
    | Int of int
    | App of t * t
    | Proj of t * int Spanned.t
    | If_then_else of t * t * t
    | Infix of [`Eq | `Add | `Sub | `Mul] * t * t
    | Prefix of [`Neg] * t

  (** Names that bind definitions or parameters *)
  and binder =
    string option Spanned.t

  (** Function parameters, with optional type annotations *)
  and param =
    binder * Ty.t option

  (** Functions, with an optional type annotation *)
  and fun_ =
    param list * Ty.t option * t

  (** Term definitions that might be parameterised by a series of types *)
  and def =
    binder * Ty.binder list * fun_

end


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check_ty : Ty.t -> (Core.Ty.t, (Span.t * string) list) result
  val check_tm : Tm.t -> Core.Ty.t -> (Core.Tm.t, (Span.t * string) list) result
  val infer_tm : Tm.t -> (Core.Tm.t * Core.Ty.t, (Span.t * string) list) result

end = struct

  (** {2 Elaboration context} *)

  module Ctx : sig

    type t

    val create : unit -> t

    val fresh_meta : t -> Span.t -> string -> Core.Ty.t

    val extend_tys : t -> Core.Ty.name list -> t
    val extend_tm : t -> Core.Tm.name -> Core.(Ty.name list * Ty.t) -> t

    val lookup_ty : t -> string -> unit option
    val lookup_tm : t -> string -> (Core.Tm.index * Core.(Ty.name list * Ty.t)) option

    val unsolved_metas : t -> (Span.t * string) Seq.t

  end = struct

    type t = {
      ty_names : Core.Ty.name list list;
      (** A stack of types currently in scope *)

      tm_tys : (Core.Tm.name * Core.(Ty.name list * Ty.t)) list;
      (** A stack of term bindings currently in scope *)

      metas : (Core.Ty.meta * Span.t * string) Dynarray.t;
      (** A list of the metavariables that have been inserted during elaboration.
          This will be used to generate a list of unsolved metavariables once
          elaboration is complete. *)
    }

    let create () = {
      ty_names = [];
      tm_tys = [];
      metas = Dynarray.create ();
    }

    let fresh_meta (ctx : t) (span : Span.t) (desc : string) : Core.Ty.t =
      let meta = Core.Ty.fresh_meta () in
      Dynarray.add_last ctx.metas (meta, span, desc);
      Core.Ty.Meta_var meta

    let extend_tys (ctx : t) (names : Core.Ty.name list) : t =
      { ctx with ty_names = names :: ctx.ty_names }

    let extend_tm (ctx : t) (name : Core.Tm.name) (ty_params, ty : Core.(Ty.name list * Ty.t)) : t =
      { ctx with tm_tys = (name, (ty_params, ty)) :: ctx.tm_tys }

    let lookup_ty (ctx : t) (name : string) : unit option =
      if List.exists (List.mem name) ctx.ty_names then Some () else None

    let lookup_tm (ctx : t) (name : string) : (Core.Tm.index * Core.(Ty.name list * Ty.t)) option =
      ctx.tm_tys |> List.find_mapi @@ fun index (name', ty) ->
        match Some name = name' with
        | true -> Some (index, ty)
        | false -> None

    let unsolved_metas (ctx : t) : (Span.t * string) Seq.t =
      Dynarray.to_seq ctx.metas |> Seq.filter_map @@ function
        | { contents = Core.Ty.Unsolved _ }, span, desc -> Some (span, desc)
        | { contents = Core.Ty.Solved _ }, _, _ -> None

  end


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of Span.t * string

  (** Raise an elaboration error with a formatted message *)
  let error (type a b) (span : Span.t) : (b, Format.formatter, unit, a) format4 -> b =
    Format.kasprintf (fun message -> raise (Error (span, message)))

  let unify_tys (span : Span.t) ~(found : Core.Ty.t) ~(expected : Core.Ty.t) =
    try Core.Ty.unify found expected with
    | Core.Ty.Infinite_type _ -> error span "infinite type"
    | Core.Ty.Mismatched_types (_, _) ->
        error span "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (Core.Ty.pp expected)
          (Core.Ty.pp found)


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ctx : Ctx.t) (ty : Ty.t) : Core.Ty.t =
    match ty.data with
      | Ty.Name name ->
          begin match Ctx.lookup_ty ctx name with
          | Some () -> Core.Ty.Var name
          | None when name = "Bool" -> Core.Ty.Bool
          | None when name = "Int" -> Core.Ty.Int
          | None -> error ty.span "unbound type name `%s`" name
          end
    | Ty.Fun (ty1, ty2) ->
        Core.Ty.Fun (check_ty ctx ty1, check_ty ctx ty2)
    | Ty.Tuple elem_tys ->
        Core.Ty.Tuple (List.map (check_ty ctx) elem_tys)
    | Ty.Placeholder ->
        Ctx.fresh_meta ctx ty.span "placeholder"

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : Ctx.t) (tm : Tm.t) (ty : Core.Ty.t) : Core.Tm.t =
    match tm.data, Core.Ty.force ty with
    | Tm.Let (None, [], body), body_ty ->
        check_tm ctx body body_ty

    | Tm.Let (None, def :: defs, body), body_ty ->
        let ctx, def = check_def ctx def in
        let body = check_tm ctx { tm with data = Let (None, defs, body) } body_ty in
        Core.Tm.Let (def, body)

    | Tm.Let (Some `Rec, defs, body), body_ty ->
        let ctx, defs = check_rec_defs ctx defs in
        let body = check_tm ctx body body_ty in
        Core.Tm.Let_rec (defs, body)

    | Tm.Fun fun_, fun_ty ->
        check_fun ctx tm.span fun_ fun_ty

    | Tm.Tuple elems, Core.Ty.Tuple elem_tys ->
        if List.length elems <> List.length elem_tys then
          error tm.span "expected %i elements, found %i elements"
            (List.length elem_tys)
            (List.length elems);
        Core.Tm.Tuple_lit (List.map2 (check_tm ctx) elems elem_tys)

    | Tm.If_then_else (head, tm1, tm2), body_ty ->
        let head = check_tm ctx head Core.Ty.Bool in
        let tm1 = check_tm ctx tm1 body_ty in
        let tm2 = check_tm ctx tm2 body_ty in
        Core.Tm.Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _, _ ->
        let tm', ty' = infer_tm ctx tm in
        unify_tys tm.span ~found:ty' ~expected:ty;
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : Ctx.t) (tm : Tm.t) : Core.Tm.t * Core.Ty.t =
    match tm.data with
    | Tm.Name ({ span; data = name }, ty_args) ->
        let tm, pty =
          match Ctx.lookup_tm ctx name with
          | Some (index, pty) -> Core.((fun ty_args -> Tm.Var (index, ty_args)), pty)
          | None when name = "true" -> Core.((fun[@warning "-partial-match"] [] -> Tm.Bool_lit true), ([], Ty.Bool))
          | None when name = "false" -> Core.((fun[@warning "-partial-match"] [] -> Tm.Bool_lit false), ([], Ty.Bool))
          | None -> error tm.span "unbound name `%s`" name
        in

      begin match pty, ty_args with
      (* No explicit type parameters supplied, so instantiate them with fresh
         metavariables *)
      | (ty_params, ty), [] ->
          let mapping =
            ty_params |> List.map @@ fun name ->
              name, Ctx.fresh_meta ctx span "type argument"
          in
          tm (List.map snd mapping), Core.Ty.subst mapping ty

      (* Explicit type arguments were provided *)
      | (ty_params, ty), ty_args when List.length ty_params = List.length ty_args ->
          let ty_args = List.map (check_ty ctx) ty_args in
          let mapping = List.combine ty_params ty_args in
          tm (List.map snd mapping), Core.Ty.subst mapping ty

      | (ty_params, _), ty_args ->
          error span "expected %i type %s, found %i"
            (List.length ty_params)
            (match ty_params with [_] -> "argument" | _ -> "arguments")
            (List.length ty_args)
      end

    | Tm.Let (None, [], body) ->
        infer_tm ctx body

    | Tm.Let (None, def :: defs, body) ->
        let ctx, def = check_def ctx def in
        let body, body_ty = infer_tm ctx { tm with data = Let (None, defs, body) } in
        Core.Tm.Let (def, body), body_ty

    | Tm.Let (Some `Rec, defs, body) ->
        let ctx, def = check_rec_defs ctx defs in
        let body, body_ty = infer_tm ctx body in
        Core.Tm.Let_rec (def, body), body_ty

    | Tm.Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        check_tm ctx tm ty, ty

    | Tm.Fun fun_ ->
        infer_fun ctx fun_

    | Tm.Tuple elems ->
        let elems, elem_tys = List.split (List.map (infer_tm ctx) elems) in
        Core.Tm.Tuple_lit elems, Core.Ty.Tuple elem_tys

    | Tm.Int i ->
        Core.Tm.Int_lit i, Core.Ty.Int

    | Tm.App ({ span = head_span; _ } as head, arg) ->
        let head, head_ty = infer_tm ctx head in
        begin match Core.Ty.force head_ty with
        | Core.Ty.Fun (param_ty, body_ty) ->
            let arg = check_tm ctx arg param_ty in
            Core.Tm.Fun_app (head, arg), body_ty
        | Core.Ty.Meta_var _ as head_ty ->
            let arg_ty = Ctx.fresh_meta ctx arg.span "function argument" in
            let body_ty = Ctx.fresh_meta ctx head_span "function return type" in
            unify_tys head_span ~found:head_ty ~expected:(Core.Ty.Fun (arg_ty, body_ty));
            let arg = check_tm ctx arg arg_ty in
            Core.Tm.Fun_app (head, arg), body_ty
        | _ -> error arg.span "unexpected argument"
        end

    | Tm.If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Core.Ty.Bool in
        let ty = Ctx.fresh_meta ctx tm.span "if branches" in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Core.Tm.Bool_elim (head, tm1, tm2), ty

    | Tm.Infix (`Eq, tm1, tm2) ->
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        unify_tys tm.span ~found:ty2 ~expected:ty1;
        begin match Core.Ty.force ty1 with
        | Core.Ty.Bool -> Core.Tm.Prim_app (Prim.Bool_eq, [tm1; tm2]), Core.Ty.Bool
        | Core.Ty.Int -> Core.Tm.Prim_app (Prim.Int_eq, [tm1; tm2]), Core.Ty.Bool
        | ty -> error tm.span "@[unsupported type: %t@]" (Core.Ty.pp ty)
        end

    | Tm.Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Core.Ty.Int in
        let tm2 = check_tm ctx tm2 Core.Ty.Int in
        Core.Tm.Prim_app (prim, [tm1; tm2]), Core.Ty.Int

    | Tm.Proj ({ span = head_span; _ } as head, index) ->
        let head, head_ty = infer_tm ctx head in
        begin match Core.Ty.force head_ty with
        | Core.Ty.Tuple elem_tys ->
            begin match List.nth_opt elem_tys index.data with
            | Some ty -> Core.Tm.Tuple_proj (head, index.data), ty
            | None -> error index.span "unknown field `%i`" index.data
            end
        | _ -> error head_span "@[expected tuple, found: %t@]" (Core.Ty.pp head_ty)
        end

    | Tm.Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Core.Ty.Int in
        Core.Tm.Prim_app (Prim.Int_neg, [tm]), Core.Ty.Int

  (** Elaborate a function into a core term, given an expected type. *)
  and check_fun (ctx : Ctx.t) (span : Span.t) (params, body_ty, body : Tm.fun_) (fun_ty : Core.Ty.t) : Core.Tm.t =
    match params, body_ty, Core.Ty.force fun_ty with
    | [], None, body_ty ->
        check_tm ctx body body_ty
    | [], Some ({ span = body_ty_span; _ } as body_ty), body_ty' ->
        let body_ty = check_ty ctx body_ty in
        unify_tys body_ty_span ~found:body_ty ~expected:body_ty';
        check_tm ctx body body_ty
    | (name, None) :: params, body_ty, Core.Ty.Fun (param_ty, ty) ->
        let body = check_fun (Ctx.extend_tm ctx name.data ([], param_ty)) span (params, body_ty, body) ty in
        Core.Tm.Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, body_ty, Core.Ty.Fun (param_ty', ty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        unify_tys param_ty_span ~found:param_ty ~expected:param_ty';
        let body = check_fun (Ctx.extend_tm ctx name.data ([], param_ty)) span (params, body_ty, body) ty in
        Core.Tm.Fun_lit (name.data, param_ty, body)
    | (_ :: _) as params, body_ty, Core.Ty.Meta_var _ ->
        let fun', fun_ty' = infer_fun ctx (params, body_ty, body) in
        unify_tys span ~found:fun_ty' ~expected:fun_ty;
        fun'
    | (name, _) :: _, _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function into a core term, inferring its type. *)
  and infer_fun (ctx : Ctx.t) (params, body_ty, body : Tm.fun_) : Core.Tm.t * Core.Ty.t =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, param_ty) :: params, body_ty ->
        let param_ty =
          match param_ty with
          | None -> Ctx.fresh_meta ctx name.span "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let body, body_ty =
          infer_fun (Ctx.extend_tm ctx name.data ([], param_ty)) (params, body_ty, body)
        in
        Core.Tm.Fun_lit (name.data, param_ty, body), Core.Ty.Fun (param_ty, body_ty)

  and check_ty_params (ty_params : Ty.binder list) : Core.Ty.name list =
    ListLabels.fold_left ty_params ~init:[]
      ~f:(fun names Spanned.{ span; data = name } ->
        if not (List.mem name names) then name :: names else
          error span "reused type parameter name %s" name)
    |> List.rev

  (** Elaborate a definition, and add it to the context. *)
  and check_def (ctx : Ctx.t) (name, ty_params, fun_ : Tm.def) : Ctx.t * Core.Tm.def =
    let ty_params = check_ty_params ty_params in
    let tm, ty = infer_fun (Ctx.extend_tys ctx ty_params) fun_ in
    Ctx.extend_tm ctx name.data (ty_params, ty), (name.data, ty_params, ty, tm)

  (** Elaborate a series of mutually recursive definitions, returning a
      context with them bound. *)
  and check_rec_defs (ctx : Ctx.t) (defs : Tm.def list) : Ctx.t * Core.Tm.def list =
    (* Elaborate the type of a function, generating fresh metavariables for
       any missing annotations *)
    let rec check_fun_ty ctx span params body_ty =
      match params, body_ty with
      | [], None -> Ctx.fresh_meta ctx span "definition type"
      | [], Some body_ty -> check_ty ctx body_ty
      | (name, None : Tm.param) :: params, body_ty ->
          let param_ty = Ctx.fresh_meta ctx name.span "parameter type" in
          Core.Ty.Fun (param_ty, check_fun_ty ctx span params body_ty)
      | (_, Some param_ty : Tm.param) :: params, body_ty ->
          let param_ty = check_ty ctx param_ty in
          Core.Ty.Fun (param_ty, check_fun_ty ctx span params body_ty)
    in

    (* Elaborate a function against an expected type *)
    let rec check_fun ctx params body fun_ty =
      match params, Core.Ty.force fun_ty with
      | [], body_ty -> check_tm ctx body body_ty
      | (name, _ : Tm.param) :: params, Core.Ty.Fun (param_ty, body_ty) ->
          let body = check_fun (Ctx.extend_tm ctx name.data ([], param_ty)) params body body_ty in
          Core.Tm.Fun_lit (name.data, param_ty, body)
      | (name, _ : Tm.param) :: _, _ -> error name.span "unexpected parameter"
    in

    (* Find the forward declarations for each recursive definition, adding them
       to the context *)
    let ~ctx, ~seen =
      ListLabels.fold_left defs
        ~init:(~ctx, ~seen:[])
        ~f:(fun (~ctx, ~seen) (name, ty_params, (params, body_ty, _) : Tm.def) ->
          match name.data with
          | None -> error name.span "placeholder in recursive binding"
          | Some n when Option.is_some (List.assoc_opt n seen) ->
              error name.span "reused name `%s` in recursive binding" n
          | Some n ->
              let ty_params = check_ty_params ty_params in
              let ty = check_fun_ty (Ctx.extend_tys ctx ty_params) name.span params body_ty in
              ~ctx:(Ctx.extend_tm ctx (Some n) (ty_params, ty)),
              ~seen:((n, (ty_params, ty)) :: seen))
    in

    (* Elaborate the definitions with the recursive definitions in scope *)
    let defs =
      defs |> List.map @@ fun (name, _, (params, _, body) : Tm.def) ->
        let ty_params, body_ty = List.assoc (Option.get name.data) seen in
        let tm = check_fun (Ctx.extend_tys ctx ty_params) params body body_ty in

        (* Avoid “vicious circles” with a blunt syntactic check on the elaborated
          term, ensuring that it is a function literal. This is similar to the
          approach used in Standard ML. This rules out definitions like:

              let x := x + 1;

          OCaml uses a more complicated check (as of versions 4.06 and 4.08) that
          admits more valid programs. A detailed description of this check can be
          found in “A practical mode system for recursive definitions” by Reynaud
          et. al. https://doi.org/10.1145/3434326. *)
        match tm with
        | Core.Tm.Fun_lit _ as tm -> name.data, ty_params, body_ty, tm
        | _ -> error name.span "definitions must be functions in recursive let bindings"
    in

    ctx, defs


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : Ctx.t -> a) : (a, (Span.t * string) list) result =
    match
      let ctx = Ctx.create () in
      let result = prog ctx in
      let ambiguity_errors =
        Ctx.unsolved_metas ctx
        |> Seq.map (fun (span, info) -> span, "ambiguous " ^ info)
        |> List.of_seq
      in
      result, ambiguity_errors
    with
    | result, [] -> Ok result
    | _, errors -> Error errors
    | exception Error (span, message) -> Error [(span, message)]


  (** {2 Public API} *)

  let check_ty (ty : Ty.t) : (Core.Ty.t, (Span.t * string) list) result =
    run_elab (fun ctx -> check_ty ctx ty)

  let check_tm (tm : Tm.t) (ty : Core.Ty.t) : (Core.Tm.t, (Span.t * string) list) result =
    run_elab (fun ctx -> check_tm ctx tm ty)

  let infer_tm (tm : Tm.t) : (Core.Tm.t * Core.Ty.t, (Span.t * string) list) result =
    run_elab (fun ctx -> infer_tm ctx tm)

end
