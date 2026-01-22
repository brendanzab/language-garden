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
    | Name of string Spanned.t * t list
    | Fun of t * t
    | Tuple of t list
    | Placeholder

  (** Names that bind type definitions or parameters *)
  and binder =
    string option Spanned.t

  (** Type definitions that might be parameterised by a series of types *)
  and def =
    binder * binder list * t

end

(** Terms in the surface language *)
module Tm = struct

  type t =
    data Spanned.t

  and data =
    | Name of string Spanned.t * Ty.t list
    | Prim of string
    | Let of [`Rec] option * def list * t
    | Let_type of Ty.def * t
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

    module Ty := Core.Ty
    module Tm := Core.Tm

    type t

    val create : unit -> t

    val fresh_ty : t -> Span.t -> string -> Ty.t
    val fresh_vty : t -> Span.t -> string -> Ty.Value.t

    val extend_ty_param : t -> Core.name -> t
    val extend_poly_ty : t -> Core.name -> Ty.Clos.t -> t
    val lookup_ty : t -> string -> (Core.index * Ty.Clos.t) option

    val extend_poly_tm : t -> Core.name -> Ty.Clos.t -> t
    val extend_tm : t -> Core.name -> Ty.Value.t -> t
    val lookup_tm : t -> string -> (Core.index * Ty.Clos.t) option

    val eval_ty : t -> Ty.t -> Ty.Value.t
    val quote_vty : t -> Ty.Value.t -> Ty.t
    val close_ty : t -> Core.name list -> Ty.t -> Ty.Clos.t
    val pp_vty : t -> Ty.Value.t -> Format.formatter -> unit

    val unsolved_metas : t -> (Span.t * string) Seq.t

  end = struct

    module Ty = Core.Ty
    module Tm = Core.Tm

    type t = {
      ty_size : Core.level;
      ty_names : Core.name Core.Env.t;
      ty_defs : Ty.Clos.t Core.Env.t;

      tm_names : Core.name Core.Env.t;
      tm_tys : Ty.Clos.t Core.Env.t;

      metas : (Ty.meta * Span.t * string) Dynarray.t;
      (** A list of the metavariables that have been inserted during elaboration.
          This will be used to generate a list of unsolved metavariables once
          elaboration is complete. *)
    }

    let create () = {
      ty_size = 0;
      ty_names = Core.Env.empty;
      ty_defs = Core.Env.empty;
      tm_names = Core.Env.empty;
      tm_tys = Core.Env.empty;
      metas = Dynarray.create ();
    }

    let fresh_vty (ctx : t) (span : Span.t) (desc : string) : Ty.Value.t =
      let meta = Ty.fresh_meta () in
      Dynarray.add_last ctx.metas (meta, span, desc);
      Ty.Value.Meta meta

    let fresh_ty (ctx : t) (span : Span.t) (desc : string) : Ty.t =
      Ty.quote ctx.ty_size (fresh_vty ctx span desc)

    let extend_poly_ty (ctx : t) (name : Core.name) (cty : Ty.Clos.t) : t =
      { ctx with
        ty_size = 1 + ctx.ty_size;
        ty_names = Core.Env.extend name ctx.ty_names;
        ty_defs = Core.Env.extend cty ctx.ty_defs;
      }

    let extend_ty (ctx : t) (name : Core.name) (ty : Ty.Value.t) : t =
      extend_poly_ty ctx name (Ty.Clos.value ty)

    let extend_ty_param (ctx : t) (name : Core.name) : t =
      extend_ty ctx name (Ty.Value.Var ctx.ty_size)

    let lookup_ty (ctx : t) (name : string) : (Core.index * Ty.Clos.t) option =
      Core.Env.find (Option.fold ~some:((=) name) ~none:false) ctx.ty_names
      |> Option.map (fun index -> index, Core.Env.lookup index ctx.ty_defs)

    let extend_poly_tm (ctx : t) (name : Core.name) (cty : Ty.Clos.t) : t =
      { ctx with
        tm_names = Core.Env.extend name ctx.tm_names;
        tm_tys = Core.Env.extend cty ctx.tm_tys;
      }

    let extend_tm (ctx : t) (name : Core.name) (ty : Ty.Value.t) : t =
      extend_poly_tm ctx name (Ty.Clos.value ty)

    let lookup_tm (ctx : t) (name : string) : (Core.index * Ty.Clos.t) option =
      Core.Env.find (Option.fold ~some:((=) name) ~none:false) ctx.tm_names
      |> Option.map (fun index -> index, Core.Env.lookup index ctx.tm_tys)

    let eval_ty (ctx : t) (ty : Ty.t) : Ty.Value.t =
      Ty.eval ctx.ty_defs ty

    let quote_vty (ctx : t) (vty : Ty.Value.t) : Ty.t =
      Ty.quote ctx.ty_size vty

    let close_ty (ctx : t) (ty_params : Core.name list) (ty : Ty.t) : Ty.Clos.t =
      Ty.Clos.make ctx.ty_defs (List.length ty_params) ty

    let pp_ty (ctx : t) (ty : Ty.t) (ppf : Format.formatter) : unit =
      Ty.pp ctx.ty_names ty ppf

    let pp_vty (ctx : t) (vty : Ty.Value.t) (ppf : Format.formatter) : unit =
      pp_ty ctx (quote_vty ctx vty) ppf

    let unsolved_metas (ctx : t) : (Span.t * string) Seq.t =
      Dynarray.to_seq ctx.metas |> Seq.filter_map @@ function
        | { contents = Ty.Unsolved _ }, span, desc -> Some (span, desc)
        | { contents = Ty.Solved _ }, _, _ -> None

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

  let unify_vtys (ctx : Ctx.t) (span : Span.t) ~(found : Core.Ty.Value.t) ~(expected : Core.Ty.Value.t) =
    try Core.Ty.Value.unify found expected with
    | Core.Ty.Value.Infinite_type -> error span "infinite type"
    | Core.Ty.Value.Mismatched_types ->
        error span "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (Ctx.pp_vty ctx expected)
          (Ctx.pp_vty ctx found)


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
      | Ty.Name ({ span; data = name }, ty_args) ->
          let arity, ty =
            match Ctx.lookup_ty ctx name with
            | Some (index, cty) -> Core.Ty.(Clos.arity cty, fun ty_args -> Var (index, ty_args))
            | None when name = "Bool" -> Core.Ty.(0, fun _ -> Bool)
            | None when name = "Int" -> Core.Ty.(0, fun _ -> Int)
            | None -> error ty.span "unbound type name `%s`" name
          in

          if arity = List.length ty_args then
            ty (List.map (check_ty ctx) ty_args)
          else
            error span "expected %i type %s, found %i"
              arity
              (if arity = 1 then "argument" else "arguments")
              (List.length ty_args)

    | Ty.Fun (ty1, ty2) ->
        Core.Ty.Fun (check_ty ctx ty1, check_ty ctx ty2)
    | Ty.Tuple elem_tys ->
        Core.Ty.Tuple (List.map (check_ty ctx) elem_tys)
    | Ty.Placeholder ->
        Ctx.fresh_ty ctx ty.span "placeholder"

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : Ctx.t) (tm : Tm.t) (ty : Core.Ty.Value.t) : Core.Tm.t =
    match tm.data, Core.Ty.Value.force ty with
    | Tm.Let (None, [], body), body_ty ->
        check_tm ctx body body_ty

    | Tm.Let (None, def :: defs, body), body_vty ->
        let ctx, def = check_def ctx def in
        let body = check_tm ctx { tm with data = Let (None, defs, body) } body_vty in
        Core.Tm.Let (def, body)

    | Tm.Let (Some `Rec, defs, body), body_vty ->
        let ctx, defs = check_rec_defs ctx defs in
        let body = check_tm ctx body body_vty in
        Core.Tm.Let_rec (defs, body)

    | Tm.Let_type (ty_def, body), body_vty ->
        let ctx, ty_def = infer_ty_def ctx ty_def in
        let body = check_tm ctx body body_vty in
        Core.Tm.Let_type (ty_def, body)

    | Tm.Fun fun_, fun_vty ->
        check_fun ctx tm.span fun_ fun_vty

    | Tm.Tuple elems, Core.Ty.Value.Tuple elem_vtys ->
        if List.length elems <> List.length elem_vtys then
          error tm.span "expected %i elements, found %i elements"
            (List.length elem_vtys)
            (List.length elems);
        Core.Tm.Tuple_lit (List.map2 (check_tm ctx) elems elem_vtys)

    | Tm.If_then_else (head, tm1, tm2), body_vty ->
        let head = check_tm ctx head Core.Ty.Value.Bool in
        let tm1 = check_tm ctx tm1 body_vty in
        let tm2 = check_tm ctx tm2 body_vty in
        Core.Tm.Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _, expected_vty ->
        let tm', found_vty = infer_tm ctx tm in
        unify_vtys ctx tm.span ~found:found_vty ~expected:expected_vty;
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : Ctx.t) (tm : Tm.t) : Core.Tm.t * Core.Ty.Value.t =
    match tm.data with
    | Tm.Name ({ span; data = name }, ty_args) ->
        let tm, cty =
          match Ctx.lookup_tm ctx name with
          | Some (index, cty) -> Core.((fun ty_args -> Tm.Var (index, ty_args)), cty)
          | None when name = "true" -> Core.((fun _ -> Tm.Bool_lit true), Ty.Clos.value Ty.Value.Bool)
          | None when name = "false" -> Core.((fun _ -> Tm.Bool_lit false), Ty.Clos.value Ty.Value.Bool)
          | None -> error tm.span "unbound name `%s`" name
        in

      begin match cty, ty_args with
      (* No explicit type parameters supplied, so instantiate them with fresh
         metavariables *)
      | cty, [] ->
          let ty_args =
            List.init (Core.Ty.Clos.arity cty) @@ fun _ ->
              Ctx.fresh_vty ctx span "type argument"
          in
          tm (List.map (Ctx.quote_vty ctx) ty_args), Core.Ty.Clos.inst cty ty_args

      (* Explicit type arguments were provided *)
      | cty, ty_args when Core.Ty.Clos.arity cty = List.length ty_args ->
          let ty_args = List.map (check_ty ctx) ty_args in
          tm ty_args, Core.Ty.Clos.inst cty (List.map (Ctx.eval_ty ctx) ty_args)

      | cty, ty_args ->
          error span "expected %i type %s, found %i"
            (Core.Ty.Clos.arity cty)
            (if Core.Ty.Clos.arity cty = 1 then "argument" else "arguments")
            (List.length ty_args)
      end

    | Prim name ->
        begin match Prim.of_name name with
        | Some (Bool_eq as prim) -> Core.Tm.Prim prim, Core.Ty.Value.(Fun (Bool, Fun (Bool, Bool)))
        | Some (Int_eq as prim) -> Core.Tm.Prim prim, Core.Ty.Value.(Fun (Int, Fun (Int, Bool)))
        | Some (Int_add as prim) -> Core.Tm.Prim prim, Core.Ty.Value.(Fun (Int, Fun (Int, Int)))
        | Some (Int_sub as prim) -> Core.Tm.Prim prim, Core.Ty.Value.(Fun (Int, Fun (Int, Int)))
        | Some (Int_mul as prim) -> Core.Tm.Prim prim, Core.Ty.Value.(Fun (Int, Fun (Int, Int)))
        | Some (Int_neg as prim) -> Core.Tm.Prim prim, Core.Ty.Value.(Fun (Int, Int))
        | None -> error tm.span "unknown primitive operation `#%s`" name
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

    | Tm.Let_type (ty_def, body) ->
        let ctx, ty_def = infer_ty_def ctx ty_def in
        let body, body_vty = infer_tm ctx body in
        Core.Tm.Let_type (ty_def, body), body_vty

    | Tm.Ann (tm, ty) ->
        let ty = check_ty ctx ty in
        let vty = Ctx.eval_ty ctx ty in
        check_tm ctx tm vty, vty

    | Tm.Fun fun_ ->
        let tm, ty = infer_fun ctx fun_ in
        tm, Ctx.eval_ty ctx ty

    | Tm.Tuple elems ->
        let elems, elem_vtys = List.split (List.map (infer_tm ctx) elems) in
        Core.Tm.Tuple_lit elems, Core.Ty.Value.Tuple elem_vtys

    | Tm.Int i ->
        Core.Tm.Int_lit i, Core.Ty.Value.Int

    | Tm.App ({ span = head_span; _ } as head, arg) ->
        let head, head_vty = infer_tm ctx head in
        begin match Core.Ty.Value.force head_vty with
        | Core.Ty.Value.Fun (param_vty, body_vty) ->
            let arg = check_tm ctx arg param_vty in
            Core.Tm.Fun_app (head, arg), body_vty
        | Core.Ty.Value.Meta _ as head_vty ->
            let arg_vty = Ctx.fresh_vty ctx arg.span "function argument" in
            let body_vty = Ctx.fresh_vty ctx head_span "function return type" in
            unify_vtys ctx head_span ~found:head_vty ~expected:(Core.Ty.Value.Fun (arg_vty, body_vty));
            let arg = check_tm ctx arg arg_vty in
            Core.Tm.Fun_app (head, arg), body_vty
        | _ -> error arg.span "unexpected argument"
        end

    | Tm.If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Core.Ty.Value.Bool in
        let body_vty = Ctx.fresh_vty ctx tm.span "if branches" in
        let tm1 = check_tm ctx tm1 body_vty in
        let tm2 = check_tm ctx tm2 body_vty in
        Core.Tm.Bool_elim (head, tm1, tm2), body_vty

    | Tm.Infix (`Eq, tm1, tm2) ->
        let tm1, vty1 = infer_tm ctx tm1 in
        let tm2, vty2 = infer_tm ctx tm2 in
        unify_vtys ctx tm.span ~found:vty2 ~expected:vty1;
        begin match Core.Ty.Value.force vty1 with
        | Core.Ty.Value.Bool -> Core.Tm.(fun_app (Prim Prim.Bool_eq) [tm1; tm2]), Core.Ty.Value.Bool
        | Core.Ty.Value.Int -> Core.Tm.(fun_app (Prim Prim.Int_eq) [tm1; tm2]), Core.Ty.Value.Bool
        | vty -> error tm.span "@[unsupported type: %t@]" (Ctx.pp_vty ctx vty)
        end

    | Tm.Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Core.Ty.Value.Int in
        let tm2 = check_tm ctx tm2 Core.Ty.Value.Int in
        Core.Tm.(fun_app (Prim prim) [tm1; tm2]), Core.Ty.Value.Int

    | Tm.Proj ({ span = head_span; _ } as head, index) ->
        let head, head_vty = infer_tm ctx head in
        begin match Core.Ty.Value.force head_vty with
        | Core.Ty.Value.Tuple elem_vtys ->
            begin match List.nth_opt elem_vtys index.data with
            | Some ty -> Core.Tm.Tuple_proj (head, index.data), ty
            | None -> error index.span "unknown field `%i`" index.data
            end
        | _ -> error head_span "@[expected tuple, found: %t@]" (Ctx.pp_vty ctx head_vty)
        end

    | Tm.Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Core.Ty.Value.Int in
        Core.Tm.(fun_app (Prim Prim.Int_neg) [tm]), Core.Ty.Value.Int

  (** Elaborate a function into a core term, given an expected type. *)
  and check_fun (ctx : Ctx.t) (span : Span.t) (params, body_ty, body : Tm.fun_) (vty : Core.Ty.Value.t) : Core.Tm.t =
    match params, body_ty, Core.Ty.Value.force vty with
    | [], None, body_vty ->
        check_tm ctx body body_vty
    | [], Some ({ span = body_ty_span; _ } as body_ty), body_vty' ->
        let body_ty = check_ty ctx body_ty in
        unify_vtys ctx body_ty_span ~found:(Ctx.eval_ty ctx body_ty) ~expected:body_vty';
        check_tm ctx body body_vty'
    | (name, None) :: params, body_ty, Core.Ty.Value.Fun (param_vty, vty) ->
        let body = check_fun (Ctx.extend_tm ctx name.data param_vty) span (params, body_ty, body) vty in
        Core.Tm.Fun_lit (name.data, Ctx.quote_vty ctx param_vty, body)
    | (name, Some param_ty) :: params, body_ty, Core.Ty.Value.Fun (param_vty', vty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty ctx param_ty in
        let param_vty = Ctx.eval_ty ctx param_ty in
        unify_vtys ctx param_ty_span ~found:param_vty ~expected:param_vty';
        let body = check_fun (Ctx.extend_tm ctx name.data param_vty) span (params, body_ty, body) vty in
        Core.Tm.Fun_lit (name.data, param_ty, body)
    | (_ :: _) as params, body_ty, Core.Ty.Value.Meta _ ->
        let tm', ty' = infer_fun ctx (params, body_ty, body) in
        unify_vtys ctx span ~found:(Ctx.eval_ty ctx ty') ~expected:vty;
        tm'
    | (name, _) :: _, _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function into a core term, inferring its type. *)
  and infer_fun (ctx : Ctx.t) (params, body_ty, body : Tm.fun_) : Core.Tm.t * Core.Ty.t =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty ctx body_ty in
        let body_vty = Ctx.eval_ty ctx body_ty in
        check_tm ctx body body_vty, body_ty
    | [], None ->
        let body, body_vty = infer_tm ctx body in
        body, Ctx.quote_vty ctx body_vty
    | (name, param_ty) :: params, body_ty ->
        let param_ty =
          match param_ty with
          | None -> Ctx.fresh_ty ctx name.span "function parameter type"
          | Some ty -> check_ty ctx ty
        in
        let param_vty = Ctx.eval_ty ctx param_ty in
        let body, body_ty =
          let ctx = Ctx.extend_tm ctx name.data param_vty in
          infer_fun ctx (params, body_ty, body)
        in
        Core.Tm.Fun_lit (name.data, param_ty, body), Core.Ty.Fun (param_ty, body_ty)

  and check_ty_params (ty_params : Ty.binder list) : Core.name list =
    ListLabels.fold_left ty_params ~init:[]
      ~f:(fun names Spanned.{ span; data = name } ->
        match name with
        | Some n when List.mem name names ->
            error span "reused type parameter name %s" n
        | Some _ | None -> name :: names)
    |> List.rev

  (** Elaborate a definition, and add it to the context. *)
  and check_def (ctx : Ctx.t) (name, ty_params, fun_ : Tm.def) : Ctx.t * Core.Tm.def =
    let ty_params = check_ty_params ty_params in
    let tm, ty = infer_fun (List.fold_left Ctx.extend_ty_param ctx ty_params) fun_ in
    Ctx.extend_poly_tm ctx name.data (Ctx.close_ty ctx ty_params ty),
    (name.data, ty_params, ty, tm)

  (** Elaborate a series of mutually recursive definitions, returning a
      context with them bound. *)
  and check_rec_defs (ctx : Ctx.t) (defs : Tm.def list) : Ctx.t * Core.Tm.def list =
    (* Elaborate the type of a function, generating fresh metavariables for
       any missing annotations *)
    let rec check_fun_ty ctx span params body_ty =
      match params, body_ty with
      | [], None -> Ctx.fresh_ty ctx span "definition type"
      | [], Some body_ty -> check_ty ctx body_ty
      | (name, None : Tm.param) :: params, body_ty ->
          let param_ty = Ctx.fresh_ty ctx name.span "parameter type" in
          Core.Ty.Fun (param_ty, check_fun_ty ctx span params body_ty)
      | (_, Some param_ty : Tm.param) :: params, body_ty ->
          Core.Ty.Fun (check_ty ctx param_ty, check_fun_ty ctx span params body_ty)
    in

    (* Elaborate a function against an expected type *)
    let rec check_fun ctx params body fun_vty =
      match params, Core.Ty.Value.force fun_vty with
      | [], body_vty -> check_tm ctx body body_vty
      | (name, _ : Tm.param) :: params, Core.Ty.Value.Fun (param_vty, body_vty) ->
          let body = check_fun (Ctx.extend_tm ctx name.data param_vty) params body body_vty in
          Core.Tm.Fun_lit (name.data, Ctx.quote_vty ctx param_vty, body)
      | (name, _ : Tm.param) :: _, _ -> error name.span "unexpected parameter"
    in

    (* Find the forward declarations for each recursive definition, adding them
       to the context *)
    let ~ctx, ~seen =
      ListLabels.fold_left defs
        ~init:(~ctx, ~seen:[])
        ~f:(fun (~ctx, ~seen) ({ span; _ } as name, ty_params, (params, body_ty, _) : Tm.def) ->
          match name.data with
          | None -> error name.span "placeholder in recursive binding"
          | Some name when Option.is_some (List.assoc_opt name seen) ->
              error span "reused name `%s` in recursive binding" name
          | Some n ->
              let ty_params = check_ty_params ty_params in
              let fun_ty = check_fun_ty (List.fold_left Ctx.extend_ty_param ctx ty_params) span params body_ty in
              let fun_cty = Ctx.close_ty ctx ty_params fun_ty in
              ~ctx:(Ctx.extend_poly_tm ctx (Some n) fun_cty),
              ~seen:((n, (ty_params, fun_ty)) :: seen))
    in

    (* Elaborate the definitions with the recursive definitions in scope *)
    let defs =
      defs |> List.map @@ fun (name, _, (params, _, body) : Tm.def) ->
        let ty_params, fun_ty = List.assoc (Option.get name.data) seen in
        let tm =
          let ctx = List.fold_left Ctx.extend_ty_param ctx ty_params in
          let fun_vty = Ctx.eval_ty ctx fun_ty in
          check_fun ctx params body fun_vty
        in

        (* Avoid “vicious circles” with a blunt syntactic check on the elaborated
          term, ensuring that it is a function literal. This is similar to the
          approach used in Standard ML. This rules out definitions like:

              let x := x + 1;

          OCaml uses a more complicated check (as of versions 4.06 and 4.08) that
          admits more valid programs. A detailed description of this check can be
          found in “A practical mode system for recursive definitions” by Reynaud
          et. al. https://doi.org/10.1145/3434326. *)
        match tm with
        | Core.Tm.Fun_lit _ as tm -> name.data, ty_params, fun_ty, tm
        | _ -> error name.span "definitions must be functions in recursive let bindings"
    in

    ctx, defs
    (* let name, _, _ = List.hd defs in
    error name.span "letrec is not yet implemented" *)

  and infer_ty_def (ctx : Ctx.t) (name, ty_params, ty : Ty.def) : Ctx.t * Core.Ty.def =
    let ty_params = check_ty_params ty_params in
    let ty = check_ty (List.fold_left Ctx.extend_ty_param ctx ty_params) ty in
    Ctx.extend_poly_ty ctx name.data (Ctx.close_ty ctx ty_params ty),
    (name.data, ty_params, ty)


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
    run_elab @@ fun ctx ->
      check_ty ctx ty

  let check_tm (tm : Tm.t) (ty : Core.Ty.t) : (Core.Tm.t, (Span.t * string) list) result =
    run_elab @@ fun ctx ->
      check_tm ctx tm (Ctx.eval_ty ctx ty)

  let infer_tm (tm : Tm.t) : (Core.Tm.t * Core.Ty.t, (Span.t * string) list) result =
    run_elab @@ fun ctx ->
      let tm, vty = infer_tm ctx tm in
      tm, Ctx.quote_vty ctx  vty

end
