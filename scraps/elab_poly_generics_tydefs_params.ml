(** A polymorphic functional language with parameterised type definitions.

    Extends [elab_poly_generics_tydefs.ml].

    {2 Future work}

    - Local unfolding of type definitions. See
      {{: https://andraskovacs.github.io/pdfs/wits24prez.pdf#page=19} Efficient
      Elaboration with Controlled Definition Unfolding} for more details.
    - Datatype definitions (see [misc_local_datatypes.ml] and {{: https://doi.org/10.1145/3729338}
      Practical Type Inference with Levels})
    - Defunctionalise closure representation
*)

(** Returns a list of the duplicate elements in a list *)
let find_dupes (type a) (xs : a list) : a list =
  let rec go acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs when List.mem x xs && not (List.mem x acc) -> go (x :: acc) xs
    | _ :: xs -> go acc xs
  in
  go [] xs


(** The explicitly typed core language that the surface language will be
    elaborated to. *)
module Core = struct

  (** Base datatypes describing the syntax of types. *)
  module rec Ty_data : sig

    (** Names of bound type variables *)
    type name = string

    (** De-bruijn index *)
    type index = int

    (** Types *)
    type t =
      | Var of index * t list   (* Type variables (bound by type parameters or local type definitions) *)
      | Meta of meta            (* Metavariables (used for unification) *)
      | Fun of t * t
      | Tuple of t list
      | Bool
      | Int

    (** Type definitions (used in local type definitions) *)
    and def = name * name list * t

    (** Mutable representation of metavariables, to be updated in-place during
        unification and typechecking *)
    and meta = meta_state ref

    (** Identifier used for pretty printing metavariables. *)
    and meta_id = int

    (** The current state of a metavariable. *)
    and meta_state =
      | Solved of Ty_data.Value.t
      | Unsolved of meta_id

    (** Evaluated types. *)
    module Value : sig

      (** De-bruijn level *)
      type level = int

      (** Types *)
      type t =
        | Var of level
        | Meta of meta
        | Fun of t * t
        | Tuple of t list
        | Bool
        | Int

      (** Types that can be instantiated with a series of type arguments *)
      (* TODO: Use a defunctionalised closure here *)
      type clos = name list * (t list -> t)

    end

  end = Ty_data

  module Ty : sig

    include module type of Ty_data

    (** Create a fresh, unsolved metavariable at a given level in the type
        environment *)
    val fresh_meta : unit -> meta

    module Value : sig

      include module type of Value

      (** Force any solved metavariables on the outermost part of a type. This is
          usually done before pattern matching on a type. *)
      val force : t -> t

      exception Mismatched_types
      exception Infinite_type

      (** Check that two types are the same, wile updating unsolved
          metavariables with known type information as required. *)
      val unify : t -> t -> unit

    end

    val eval : Value.clos list -> t -> Value.t

    val quote : Value.level -> Value.t -> t

    (** Inline solved metavariables in types *)
    val zonk : Value.level -> t -> t

    (** Inline solved metavariables in type definitions *)
    val zonk_def : Value.level -> def -> def

    (** Pretty print a type *)
    val pp : Value.level -> name list -> t -> Format.formatter -> unit

  end = struct

    include Ty_data

    module Value = struct

      include Value

      let rec force (ty : t) : t =
        match ty with
        | Meta ({ contents = Solved ty } as m) ->
            let ty = force ty in
            m := Solved ty;
            ty
        | ty -> ty

      exception Mismatched_types
      exception Infinite_type

      (** Ensure that the candidate type does not refer to the to-be-solved
          metavariable *)
      let rec occurs (m : meta) (ty : t) =
        match ty with
        | Var _ -> ()
        | Meta m' when m == m' -> raise Infinite_type
        | Meta { contents = Solved ty } -> occurs m ty
        | Meta { contents = Unsolved _ } -> ()
        | Fun (param_ty, body_ty) ->
            occurs m param_ty;
            occurs m body_ty
        | Tuple elem_tys ->
            List.iter (occurs m) elem_tys
        | Bool -> ()
        | Int -> ()

      let rec unify (ty1 : t) (ty2 : t) =
        match force ty1, force ty2 with
        | Var name1, Var name2 when name1 = name2 -> ()
        | Meta m1, Meta m2 when m1 == m2 -> ()    (* NOTE: using pointer equality for references *)
        | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
            unify param_ty1 param_ty2;
            unify body_ty1 body_ty2
        | Tuple [], Tuple [] -> ()
        | Tuple (elem_ty1 :: elem_tys1), Tuple (elem_ty2 :: elem_tys2) ->
            unify elem_ty1 elem_ty2;
            unify (Tuple elem_tys1) (Tuple elem_tys2)
        | Bool, Bool -> ()
        | Int, Int -> ()

        (* Unify through solved metavariables *)
        | Meta { contents = Solved ty1 }, ty2 -> unify ty1 ty2
        | ty1, Meta { contents = Solved ty2 } -> unify ty1 ty2

        (* Update unsolved metavariables in-place *)
        | Meta ({ contents = Unsolved _ } as m), ty
        | ty, Meta ({ contents = Unsolved _ } as m) ->
            occurs m ty;
            m := Solved ty

        | _, _ -> raise Mismatched_types

    end

    let fresh_meta : unit -> meta =
      let next = ref 0 in
      fun () ->
        let id = !next in
        incr next;
        ref (Unsolved id)

    let rec eval (env : Value.clos list) (ty : t) : Value.t =
      match ty with
      | Var (index, args) -> snd (List.nth env index) (List.map (eval env) args)
      | Meta meta -> Value.Meta meta
      | Fun (param_ty, body_ty) -> Value.Fun (eval env param_ty, eval env body_ty)
      | Tuple elem_tys -> Value.Tuple (List.map (eval env) elem_tys)
      | Bool -> Value.Bool
      | Int -> Value.Int

    let rec quote (size : Value.level) (vty : Value.t) : t =
      match vty with
      | Value.Var level -> Var (size - level - 1, [])
      | Value.Meta meta -> Meta meta
      | Value.Fun (param_ty, body_ty) -> Fun (quote size param_ty, quote size body_ty)
      | Value.Tuple elem_tys -> Tuple (List.map (quote size) elem_tys)
      | Value.Bool -> Bool
      | Value.Int -> Int

    let rec zonk (size : Value.level) (ty : t) : t =
      match ty with
      | Var _ -> ty
      | Meta { contents = Solved ty } -> zonk size (quote size ty)
      | Meta { contents = Unsolved _ } -> ty
      | Fun (param_ty, body_ty) -> Fun (zonk size param_ty, zonk size body_ty)
      | Tuple elem_tys -> Tuple (List.map (zonk size) elem_tys)
      | Bool -> ty
      | Int -> Int

    let zonk_def (ty_size : Value.level) (name, ty_params, ty : def) : def =
      name, ty_params, zonk (ty_size + List.length ty_params) ty

    let pp (size : Value.level) (names : name list) (ty : t) (ppf : Format.formatter) : unit =
      let rec pp_ty ty ppf =
        match ty with
        | Meta m -> pp_meta pp_ty m ppf
        | Fun (param_ty, body_ty) ->
            Format.fprintf ppf "%t -> %t"
              (pp_atomic_ty param_ty)
              (pp_ty body_ty)
        | ty ->
            pp_app_ty ty ppf
      and pp_app_ty ty ppf =
        match ty with
        | Var (index, args) ->
            Format.fprintf ppf "@[%s@ %a@]"
              (List.nth names index)
              (Format.pp_print_list (Fun.flip pp_ty)
                ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ "))
              args
        | ty ->
            pp_atomic_ty ty ppf
      and pp_atomic_ty ty ppf =
        match ty with
        | Var (index, []) -> Format.fprintf ppf "%s" (List.nth names index)
        | Meta m -> pp_meta pp_atomic_ty m ppf
        | Tuple [] -> Format.fprintf ppf "()"
        | Tuple elem_tys ->
            Format.fprintf ppf "@[(%a)@]"
              (Format.pp_print_list (Fun.flip pp_ty)
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
              elem_tys
        | Bool -> Format.fprintf ppf "Bool"
        | Int -> Format.fprintf ppf "Int"
        | Var (_, _ :: _) | Fun _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
      and pp_meta pp_ty m ppf =
        match !m with
        | Solved ty -> pp_ty (quote size ty) ppf
        | Unsolved id -> Format.fprintf ppf "?%i" id
      in
      pp_ty ty ppf

  end

  module Expr = struct

    (** Names of bound variables *)
    type name = string

    type t =
      | Var of name * Ty.t list
      (** A variable that is possibly applied to a series of type arguments
          (which will be applied a type lambda in a definition). *)

      | Let of def * t
      (** Let bindings that are polymorphic over a series of type parameters.
          The type parameters are bound in a forall in the type annotation, and
          in a type lambda in the definition. In System F this would look like:

          {@text[
            let foo : ∀ a₁ ... aₙ. t :=
              Λ a₁ ... aₙ. e₁
            in e₂
          ]}
      *)

      | Let_type of Ty.def * t
      (** Local type definitions. *)

      | Fun_lit of name * Ty.t * t
      | Fun_app of t * t
      | Tuple_lit of t list
      | Tuple_proj of t * int
      | Bool_lit of bool
      | Bool_if of t * t * t
      | Int_lit of int

    (** Definitions *)
    and def = name * Ty.name list * Ty.t * t

    module Value = struct

      type t =
        | Fun_lit of (t -> t)
        | Tuple_lit of t list
        | Bool_lit of bool
        | Int_lit of int

      let[@warning "-unused-value-declaration"] rec pp (vexpr : t) (ppf : Format.formatter) : unit =
        match vexpr with
        | Fun_lit _ -> Format.fprintf ppf "<function>"
        | Tuple_lit [] -> Format.fprintf ppf "()"
        | Tuple_lit elems ->
            Format.fprintf ppf "@[(%a)@]"
              (Format.pp_print_list (Fun.flip pp)
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
              elems
        | Bool_lit true -> Format.fprintf ppf "true"
        | Bool_lit false -> Format.fprintf ppf "false"
        | Int_lit i -> Format.fprintf ppf "%i" i

    end

    let rec eval (env : (name * Value.t) list) (expr : t) : Value.t =
      match expr with
      | Var (name, _) -> List.assoc name env
      | Let ((name, _, _, def), body) ->
          let vdef = eval env def in
          eval ((name, vdef) :: env) body
      | Let_type (_, body) ->
          eval env body
      | Fun_lit (name, _, body) ->
          Value.Fun_lit (fun varg ->
            eval ((name, varg) :: env) body)
      | Fun_app (head, arg) ->
          begin match eval env head with
          | Value.Fun_lit vbody -> vbody (eval env arg)
          | _ -> invalid_arg "eval"
          end
      | Tuple_lit elems ->
          Value.Tuple_lit (List.map (eval env) elems)
      | Tuple_proj (head, index) ->
          begin match eval env head with
          | Value.Tuple_lit velems -> List.nth velems index
          | _ -> invalid_arg "eval"
          end
      | Bool_lit b -> Value.Bool_lit b
      | Bool_if (head, true_body, false_body) ->
          begin match eval env head with
          | Value.Bool_lit true -> eval env true_body
          | Value.Bool_lit false -> eval env false_body
          | _ -> invalid_arg "eval"
          end
      | Int_lit i -> Value.Int_lit i

    let rec zonk (ty_size : Ty.Value.level) (expr : t) : t =
      match expr with
      | Var (name, ty_args) -> Var (name, List.map (Ty.zonk ty_size) ty_args)
      | Let (def, body) -> Let (zonk_def ty_size def, zonk ty_size body)
      | Let_type (ty_def, body) -> Let_type (Ty.zonk_def ty_size ty_def, zonk (ty_size + 1) body)
      | Fun_lit (name, ty, body) -> Fun_lit (name, Ty.zonk ty_size ty, zonk ty_size body)
      | Fun_app (head, arg) -> Fun_app (zonk ty_size head, zonk ty_size arg)
      | Tuple_lit elems -> Tuple_lit (List.map (zonk ty_size) elems)
      | Tuple_proj (head, index) -> Tuple_proj (zonk ty_size head, index)
      | Bool_lit _ -> expr
      | Bool_if (head, true_body, false_body) ->
          Bool_if (zonk ty_size head, zonk ty_size true_body, zonk ty_size false_body)
      | Int_lit _ -> expr

    and zonk_def (ty_size : Ty.Value.level) (name, ty_params, def_ty, def : def) : def =
      let ty_size = ty_size + List.length ty_params in
      name, ty_params, Ty.zonk ty_size def_ty, zonk ty_size def

  end

end


(** The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient. *)
module Surface = struct

  module Ty = struct

    type t =
      | Name of string * t list
      | Fun of t * t
      | Tuple of t list
      | Placeholder
      [@@warning "-unused-constructor"]

    and def = string * string list * t

  end

  module Expr = struct

    type t =
      | Name of string * Ty.t list
      | Let of def * t
      | Let_type of Ty.def * t
      | Ann of t * Ty.t
      | Fun of string * Ty.t option * t
      | Tuple of t list
      | Int of int
      | App of t * t
      | Proj of t * int
      | If of t * t * t
      [@@warning "-unused-constructor"]

    and def = string * string list * Ty.t option * t

  end

  (** Elaboration of the surface language into the core language.

      This is where we implement user-facing type checking, while also
      translating the surface language into the simpler, more explicit core
      language (e.g. with explicit type annotations and type applications).
  *)
  module Elab : sig

    val check_ty : Ty.t -> (Core.Ty.t, string) result [@@warning "-unused-value-declaration"]
    val infer_expr : Expr.t -> (Core.Expr.t * Core.Ty.t, string) result

  end = struct

    (** An exception raised on an elaboration error. These should be caught
        before they escape this module. *)
    exception Error of string

    (** Raise an elaboration error with a formatted message *)
    let error (type a b) (f : (b, Format.formatter, unit, a) format4) : b =
      Format.kasprintf (fun msg -> raise (Error msg)) f


    (** Elaboration contexts *)
    module Ctx : sig

      module Ty := Core.Ty
      module Expr := Core.Expr

      type t

      val create : unit -> t

      val fresh_vty : t -> string -> Ty.Value.t
      val extend_ty_def : t -> Ty.name -> Ty.Value.clos -> t
      val extend_ty_param : t -> Ty.name -> t
      val lookup_ty : t -> Ty.name -> Ty.(index * Value.clos) option

      val extend_expr : t -> Expr.name -> Ty.Value.clos -> t
      val lookup_expr : t -> Expr.name -> Ty.Value.clos option

      val eval_ty : t -> Ty.t -> Ty.Value.t
      val quote_vty : t -> Ty.Value.t -> Ty.t
      val zonk_ty : t -> Ty.t -> Ty.t
      val zonk_expr : t -> Expr.t -> Expr.t
      val pp_vty : t -> Ty.Value.t -> Format.formatter -> unit

      val unsolved_metas : t -> string Seq.t

    end = struct

      module Ty = Core.Ty
      module Expr = Core.Expr

      type t = {
        metas : (Ty.meta * string) Dynarray.t;
        ty_size : Ty.Value.level;
        ty_names : Ty.name list;
        ty_defs : Ty.Value.clos list;
        expr_tys : (Expr.name * Ty.Value.clos) list;
      }

      let create () = {
        metas = Dynarray.create ();
        ty_size = 0;
        ty_names = [];
        ty_defs = [];
        expr_tys = [];
      }

      let fresh_vty (ctx : t) (desc : string) : Ty.Value.t =
        let meta = Ty.fresh_meta () in
        Dynarray.add_last ctx.metas (meta, desc);
        Ty.Value.Meta meta

      let extend_ty_def (ctx : t) (name : Ty.name) (ty_def : Ty.Value.clos) : t =
        { ctx with
          ty_size = 1 + ctx.ty_size;
          ty_names = name :: ctx.ty_names;
          ty_defs = ty_def :: ctx.ty_defs;
        }

      let extend_ty_param (ctx : t) (name : Ty.name) : t =
        extend_ty_def ctx name ([], fun _ -> Ty.Value.Var ctx.ty_size)

      let lookup_ty (ctx : t) (name : Ty.name) : Ty.(index * Value.clos) option =
        List.find_index (String.equal name) ctx.ty_names
        |> Option.map (fun index -> index, List.nth ctx.ty_defs index)


      let extend_expr (ctx : t) (name : Expr.name) (ty_params, ty : Ty.Value.clos) : t =
        { ctx with expr_tys = (name, (ty_params, ty)) :: ctx.expr_tys }

      let lookup_expr (ctx : t) (name : Expr.name) : Ty.Value.clos option =
        List.assoc_opt name ctx.expr_tys


      let eval_ty (ctx : t) (ty : Ty.t) : Ty.Value.t =
        Ty.eval ctx.ty_defs ty

      let quote_vty (ctx : t) (vty : Ty.Value.t) : Ty.t =
        Ty.quote ctx.ty_size vty

      let zonk_ty (ctx : t) (vty : Ty.t) : Ty.t =
        Ty.zonk ctx.ty_size vty

      let zonk_expr (ctx : t) (vty : Expr.t) : Expr.t =
        Expr.zonk ctx.ty_size vty

      let pp_ty (ctx : t) (ty : Ty.t) (ppf : Format.formatter) : unit =
        Ty.pp ctx.ty_size ctx.ty_names ty ppf

      let pp_vty (ctx : t) (vty : Ty.Value.t) (ppf : Format.formatter) : unit =
        pp_ty ctx (quote_vty ctx vty) ppf


      let unsolved_metas (ctx : t) : string Seq.t =
        Dynarray.to_seq ctx.metas |> Seq.filter_map @@ function
          | { contents = Ty.Unsolved _ }, desc -> Some desc
          | { contents = Ty.Solved _ }, _ -> None

    end

    let unify_vtys (ctx : Ctx.t) ~(found : Core.Ty.Value.t) ~(expected : Core.Ty.Value.t) =
      try Core.Ty.Value.unify expected found with
      | Core.Ty.Value.Mismatched_types ->
          error "expected: %t, found: %t"
            (Ctx.pp_vty ctx expected)
            (Ctx.pp_vty ctx found)
      | Core.Ty.Value.Infinite_type ->
          error "infinite type"


    (* Bidirectional elaboration *)

    (** Elaborate a type, checking that it is well-formed. *)
    let rec check_ty (ctx : Ctx.t) (ty : Ty.t) : Core.Ty.t =
      match ty with
      | Ty.Name (name, ty_args) ->
          let ty_params, ty =
            match Ctx.lookup_ty ctx name with
            | Some (index, (ty_params, _)) ->
                ty_params, fun ty_args -> Core.Ty.Var (index, ty_args)
            | None when name = "Bool" -> [], fun _ -> Core.Ty.Bool
            | None when name = "Int" -> [], fun _ -> Core.Ty.Int
            | None -> error "unbound type variable `%s`" name
          in

          if List.length ty_params = List.length ty_args then
            ty (List.map (check_ty ctx) ty_args)
          else
            error "expected %i type %s, found %i"
              (List.length ty_params)
              (match ty_params with [_] -> "argument" | _ -> "arguments")
              (List.length ty_args)

      | Ty.Fun (ty1, ty2) -> Core.Ty.Fun (check_ty ctx ty1, check_ty ctx ty2)
      | Ty.Tuple elem_tys -> Core.Ty.Tuple (List.map (check_ty ctx) elem_tys)
      | Ty.Placeholder -> Ctx.quote_vty ctx (Ctx.fresh_vty ctx "placeholder")

    (** Elaborate a term, given an expected type. *)
    let rec check_expr (ctx : Ctx.t) (expr : Expr.t) (ty : Core.Ty.Value.t) : Core.Expr.t =
      match expr, Core.Ty.Value.force ty with
      | Expr.Let (def, body), body_vty ->
          let ctx, def = infer_def ctx def in
          let body = check_expr ctx body body_vty in
          Core.Expr.Let (def, body)

      | Expr.Let_type (ty_def, body), body_vty ->
          let ctx, ty_def = infer_ty_def ctx ty_def in
          let body = check_expr ctx body body_vty in
          Core.Expr.Let_type (ty_def, body)

      | Expr.Fun (name, None, body), Core.Ty.Value.Fun (param_vty, body_vty) ->
          let param_ty = Ctx.quote_vty ctx param_vty in
          let body = check_expr (Ctx.extend_expr ctx name ([], fun _ -> param_vty)) body body_vty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.Fun (name, Some param_ty, body), Core.Ty.Value.Fun (param_vty', body_vty) ->
          let param_ty = check_ty ctx param_ty in
          let param_vty = Ctx.eval_ty ctx param_ty in
          unify_vtys ctx ~found:param_vty ~expected:param_vty';
          let body = check_expr (Ctx.extend_expr ctx name ([], fun _ -> param_vty')) body body_vty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.Tuple elems, Core.Ty.Value.Tuple elem_vtys ->
          if List.length elems <> List.length elem_vtys then
            error "expected %i elements, found %i elements"
              (List.length elem_vtys)
              (List.length elems);
          Core.Expr.Tuple_lit (List.map2 (check_expr ctx) elems elem_vtys)

      | Expr.If (head, true_body, false_body), body_ty ->
          let head = check_expr ctx head Core.Ty.Value.Bool in
          let true_body = check_expr ctx true_body body_ty in
          let false_body = check_expr ctx false_body body_ty in
          Core.Expr.Bool_if (head, true_body, false_body)

      | expr, expected_ty ->
          let expr, found_ty = infer_expr ctx expr in
          unify_vtys ctx ~found:found_ty ~expected:expected_ty;
          expr

    (** Elaborate a term, inferring its type. *)
    and infer_expr (ctx : Ctx.t) (expr : Expr.t) : Core.Expr.t * Core.Ty.Value.t =
      match expr with
      | Expr.Name (name, ty_args) ->
          let expr, pty =
            match Ctx.lookup_expr ctx name with
            | Some pty -> (fun ty_args -> Core.Expr.Var (name, ty_args)), pty
            | None when name = "true" -> Core.((fun _ -> Expr.Bool_lit true), ([], fun _ -> Ty.Value.Bool))
            | None when name = "false" -> Core.((fun _ -> Expr.Bool_lit false), ([], fun _ -> Ty.Value.Bool))
            | None -> error "unbound variable `%s`" name
          in

          begin match pty, ty_args with
          | (ty_params, ty), [] ->
              let ty_args = ty_params |> List.map (fun _ -> Ctx.fresh_vty ctx "type argument") in
              expr (List.map (Ctx.quote_vty ctx) ty_args), ty ty_args

          | (ty_params, ty), ty_args when List.length ty_params = List.length ty_args ->
              let ty_args = List.map (check_ty ctx) ty_args in
              expr ty_args, ty (List.map (Ctx.eval_ty ctx) ty_args)

          | (ty_params, _), ty_args ->
              error "expected %i type %s, found %i"
                (List.length ty_params)
                (match ty_params with [_] -> "argument" | _ -> "arguments")
                (List.length ty_args)
          end

      | Expr.Let (def, body) ->
          let ctx, def = infer_def ctx def in
          let body, body_vty = infer_expr ctx body in
          Core.Expr.Let (def, body), body_vty

      | Expr.Let_type (ty_def, body) ->
          let ctx, ty_def = infer_ty_def ctx ty_def in
          let body, body_vty = infer_expr ctx body in
          Core.Expr.Let_type (ty_def, body), body_vty

      | Expr.Ann (tm, ty) ->
          let ty = check_ty ctx ty in
          let vty = Ctx.eval_ty ctx ty in
          check_expr ctx tm vty, vty

      | Expr.Fun (name, None, body) ->
          let param_vty = Ctx.fresh_vty ctx "function parameter" in
          let body, body_vty = infer_expr (Ctx.extend_expr ctx name ([], fun _ -> param_vty)) body in
          Core.Expr.Fun_lit (name, Ctx.quote_vty ctx param_vty, body),
          Core.Ty.Value.Fun (param_vty, body_vty)

      | Expr.Fun (name, Some param_ty, body) ->
          let param_ty = check_ty ctx param_ty in
          let param_vty = Ctx.eval_ty ctx param_ty in
          let body, body_vty = infer_expr (Ctx.extend_expr ctx name ([], fun _ -> param_vty)) body in
          Core.Expr.Fun_lit (name, param_ty, body),
          Core.Ty.Value.Fun (param_vty, body_vty)

      | Expr.Tuple elems ->
          let elems, elem_tys = List.split (List.map (infer_expr ctx) elems) in
          Core.Expr.Tuple_lit elems, Core.Ty.Value.Tuple elem_tys

      | Expr.Int i ->
          Core.Expr.Int_lit i, Core.Ty.Value.Int

      | Expr.App (head, arg) ->
          let head, head_vty = infer_expr ctx head in
          begin match Core.Ty.Value.force head_vty with
          | Core.Ty.Value.Fun (param_vty, body_vty) ->
              let arg = check_expr ctx arg param_vty in
              Core.Expr.Fun_app (head, arg), body_vty
          | Core.Ty.Value.Meta _ as head_vty ->
              let arg, arg_vty = infer_expr ctx arg in
              let body_vty = Ctx.fresh_vty ctx "return type" in
              unify_vtys ctx ~found:head_vty ~expected:(Core.Ty.Value.Fun (arg_vty, body_vty));
              Core.Expr.Fun_app (head, arg), body_vty
          |_ -> error "unexpected argument"
          end

      | Expr.Proj (head, index) ->
          let head, head_vty = infer_expr ctx head in
          begin match Core.Ty.Value.force head_vty with
          | Core.Ty.Value.Tuple elem_tys ->
              begin match List.nth_opt elem_tys index with
              | Some ty -> Core.Expr.Tuple_proj (head, index), ty
              | None -> error "unknown field"
              end
          | _ -> error "expected tuple, found: %t" (Ctx.pp_vty ctx head_vty)
          end

      | Expr.If (head, true_body, false_body) ->
          let head = check_expr ctx head Core.Ty.Value.Bool in
          let body_vty = Ctx.fresh_vty ctx "if branches" in
          let true_body = check_expr ctx true_body body_vty in
          let false_body = check_expr ctx false_body body_vty in
          Core.Expr.Bool_if (head, true_body, false_body), body_vty

    (** Elaborate a polymorphic definition with an optional type annotation *)
    and infer_def (ctx : Ctx.t) (name, ty_params, def_ty, def : Expr.def) : Ctx.t * Core.Expr.def =
      match find_dupes ty_params with
      | (_ :: _) as names ->
          error "type %s introduced multiple times: %s"
            (match names with [_] -> "parameter" | _ -> "parameters")
            (String.concat ", " names)
      | [] ->
          let def, def_ty =
            let ctx = List.fold_left Ctx.extend_ty_param ctx ty_params in
            match def_ty with
            | None ->
                let def, def_vty = infer_expr ctx def in
                def, Ctx.quote_vty ctx def_vty
            | Some def_ty ->
                let def_ty = check_ty ctx def_ty in
                let def_vty = Ctx.eval_ty ctx def_ty in
                check_expr ctx def def_vty, def_ty
          in
          let def_pty ty_args =
            (* FIXME: Closing over the entire context? Could be costly. *)
            let extend_ty_arg ctx name vty = Ctx.extend_ty_def ctx name ([], fun _ -> vty) in
            Ctx.eval_ty (List.fold_left2 extend_ty_arg ctx ty_params ty_args) def_ty
          in
          Ctx.extend_expr ctx name (ty_params, def_pty),
          (name, ty_params, def_ty, def)

    and infer_ty_def (ctx : Ctx.t) (name, ty_params, ty : Ty.def) : Ctx.t * Core.Ty.def =
      match find_dupes ty_params with
      | (_ :: _) as names ->
          error "type %s introduced multiple times: %s"
            (match names with [_] -> "parameter" | _ -> "parameters")
            (String.concat ", " names)
      | [] ->
          let ty =
            let ctx = List.fold_left Ctx.extend_ty_param ctx ty_params in
            check_ty ctx ty
          in
          let pty ty_args =
            (* FIXME: Closing over the entire context? Could be costly. *)
            let extend_ty_arg ctx name vty = Ctx.extend_ty_def ctx name ([], fun _ -> vty) in
            Ctx.eval_ty (List.fold_left2 extend_ty_arg ctx ty_params ty_args) ty
          in
          Ctx.extend_ty_def ctx name (ty_params, pty),
          (name, ty_params, ty)


    (** Running elaboration *)

    let run (type a) (prog : Ctx.t -> a) : (a, string) result =
      try
        let ctx = Ctx.create () in
        let result = prog ctx in
        Ctx.unsolved_metas ctx |> Seq.iter (error "ambiguous %s");
        Ok result
      with
      | Error msg -> Error msg


    (** Public API *)

    let check_ty (ty : Ty.t) : (Core.Ty.t, string) result =
      run @@ fun ctx ->
        Ctx.zonk_ty ctx (check_ty ctx ty)

    let infer_expr (expr : Expr.t) : (Core.Expr.t * Core.Ty.t, string) result =
      run @@ fun ctx ->
        let expr, vty = infer_expr ctx expr in
        Ctx.zonk_expr ctx expr,
        Ctx.(zonk_ty ctx (quote_vty ctx vty))

  end

end


let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  let open Surface in

  let ( $ ) f x = Expr.App (f, x) in

  let expect_ok result =
    match result with
    | Ok x -> x
    | Error msg -> failwith msg
  in

  begin

    (* Polymorphic identity function *)
    let expr =
      Expr.Let (("id", ["A"], None,
        Fun ("x", Some (Name ("A", [])), Name ("x", []))),
        Name ("id", []) $ Tuple [])
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let (("id", ["A"], Ty.Fun (Var (0, []), Var (0, [])),
          Fun_lit ("x", Ty.Var (0, []), Var ("x", []))),
          Fun_app (Var ("id", [Ty.Tuple []]), Tuple_lit []))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);
    end;

    (* Explicit type application *)
    let expr =
      Expr.Let (("id", ["A"], None,
        Fun ("x", Some (Name ("A", [])), Name ("x", []))),
        Name ("id", [Ty.Tuple []]) $ Tuple [])
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let (("id", ["A"], Ty.Fun (Var (0, []), Var (0, [])),
          Fun_lit ("x", Ty.Var (0, []), Var ("x", []))),
          Fun_app (Var ("id", [Ty.Tuple []]), Tuple_lit []))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);
    end;

    (* Constant function *)
    let expr =
      Expr.Let (("id", ["A"], None,
        Fun ("x", Some (Name ("A", [])), Name ("x", []))),
        Let (("const", ["A"; "B"], None,
          Fun ("x", Some (Name ("A", [])), Fun ("y", Some (Name ("B", [])), Name ("x", [])))),
          Name ("const", []) $ Tuple [] $ (Name ("id", []) $ Name ("true", []))))
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        let ( $ ) f x = Expr.Fun_app (f, x) in
        Expr.Let (("id", ["A"], Ty.Fun (Var (0, []), Var (0, [])),
          Fun_lit ("x", Ty.Var (0, []), Var ("x", []))),
          Let (("const", ["A"; "B"], Ty.Fun (Var (1, []), Ty.Fun (Var (0, []), Var (1, []))),
            Fun_lit ("x", Ty.Var (1, []), Fun_lit ("y", Ty.Var (0, []), Var ("x", [])))),
            Var ("const", [Ty.Tuple []; Ty.Bool]) $ Tuple_lit [] $ (Var ("id", [Ty.Bool]) $ Bool_lit true)))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);
    end;

    (* Locally polymorphic definitions *)
    let expr =
      Expr.Let (("kite", ["A"; "B"], Some (Fun (Name ("A", []), Fun (Name ("B", []), Name ("B", [])))),
        Fun ("x", None,
          Let (("id", ["A"], None,
            Fun ("x", Some (Name ("A", [])), Name ("x", []))),
            Name ("id", [])))),
        Name ("kite", []) $ Tuple [] $ Name ("true", []))
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        let ( $ ) f x = Expr.Fun_app (f, x) in
        Expr.Let (("kite", ["A"; "B"], Ty.Fun (Var (1, []), Ty.Fun (Var (0, []), Var (0, []))),
          Fun_lit ("x", Ty.Var (1, []),
            Let (("id", ["A"], Ty.Fun (Var (0, []), Var (0, [])),
              Fun_lit ("x", Ty.Var (0, []), Var ("x", []))),
              Var ("id", [Ty.Var (0, [])])))),
          Var ("kite", [Ty.Tuple []; Ty.Bool]) $ Tuple_lit [] $ Bool_lit true)
      ));
      assert (ty = Core.Ty.Bool);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Bool_lit true);
    end;

    (* Local type definitions *)
    let expr =
      (* False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html *)
      Expr.Let_type (("Foo", [], Name ("Int", [])),
        Ann (Int 42, Name ("Foo", [])))
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let_type (("Foo", [], Int),
          Int_lit 42)
      ));
      assert (ty = Core.Ty.Int);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Int_lit 42);
    end;

    (* Parameterised type definitions *)
    let expr =
      (* False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html *)
      Expr.Let_type (("Id", ["A"], Name ("A", [])),
        Let (("x", [], Some (Name ("Id", [Name ("Int", [])])), Int 42),
        Name ("x", [])))
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let_type (("Id", ["A"], Var (0, [])),
          Let (("x", [], Var (0, [Int]), Int_lit 42),
            Var ("x", [])))
      ));
      assert (ty = Core.Ty.Int);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Int_lit 42);
    end;

    (* TODO: More tests *)

  end;

  print_string " ok!\n";

end
