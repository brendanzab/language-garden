(** A polymorphic functional language with parameterised type definitions.

    Extends [elab_poly_generics_tydefs.ml].

    {2 Future work}

    - Local unfolding of type definitions. See
      {{: https://andraskovacs.github.io/pdfs/wits24prez.pdf#page=19} Efficient
      Elaboration with Controlled Definition Unfolding} for more details.
    - Datatype definitions (see [misc_local_datatypes.ml] and {{: https://doi.org/10.1145/3729338}
      Practical Type Inference with Levels})
*)

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

    end

  end = Ty_data

  module rec Ty : sig

    include module type of Ty_data

    (** Create a fresh, unsolved metavariable *)
    val fresh_meta : unit -> meta

    (** Evaluate a type into a value *)
    val eval : Ty.Clos.t Ty.Env.t -> t -> Ty.Value.t

    (** Quote an evaluated type back into its normal form *)
    val quote : Ty.Value.level -> Ty.Value.t -> t

    (** Inline solved metavariables in types *)
    val zonk : Ty.Value.level -> t -> t

    (** Inline solved metavariables in type definitions *)
    val zonk_def : Ty.Value.level -> def -> def

    (** Pretty print a type *)
    val pp : Ty.Value.level -> name Ty.Env.t -> t -> Format.formatter -> unit

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

    (** Types that can be instantiated with a series of type arguments. *)
    module Clos : sig

      type t

      val make : t Ty.Env.t -> int -> Ty.t -> t
      val value : Value.t -> t

      val arity : t -> int
      val inst : t -> Value.t list -> Value.t

    end

    module Env : sig

      type _ t

      val empty : 'a t
      val extend : 'a -> 'a t -> 'a t
      val lookup : index -> 'a t -> 'a
      val find : ('a -> bool) -> 'a t -> index option

    end

  end = struct

    include Ty_data

    module Env = struct

      type 'a t = 'a list

      let empty = []
      let extend = List.cons
      let lookup index env = List.nth env index
      let find = List.find_index

    end

    let fresh_meta : unit -> meta =
      let next = ref 0 in
      fun () ->
        let id = !next in
        incr next;
        ref (Unsolved id)

    (** Defunctionalised closure representation. We define {!clos} and {!inst}
        before the {!Clos} module in order to make it easier to deal with the
        mutual recursion with the {!eval} function. *)
    type clos =
      | Clos of clos Env.t * int * t
      | Value of Value.t

    let rec eval (env : clos Env.t) (ty : t) : Value.t =
      match ty with
      | Var (index, args) -> inst (Env.lookup index env) (List.map (eval env) args)
      | Meta meta -> Value.Meta meta
      | Fun (param_ty, body_ty) -> Value.Fun (eval env param_ty, eval env body_ty)
      | Tuple elem_tys -> Value.Tuple (List.map (eval env) elem_tys)
      | Bool -> Value.Bool
      | Int -> Value.Int

    and inst (cty : clos) (args : Value.t list) =
      match cty, args with
      | Clos (env, arity, body), args when List.length args = arity ->
          let extend_arg acc arg = Env.extend (Value arg) acc in
          eval (List.fold_left extend_arg env args) body
      | Value vty, [] -> vty
      | _, _ -> failwith "mismatched arity"

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

    let pp (size : Value.level) (names : name Env.t) (ty : t) (ppf : Format.formatter) : unit =
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
              (Env.lookup index names)
              (Format.pp_print_list (Fun.flip pp_ty)
                ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ "))
              args
        | ty ->
            pp_atomic_ty ty ppf
      and pp_atomic_ty ty ppf =
        match ty with
        | Var (index, []) -> Format.fprintf ppf "%s" (Env.lookup index names)
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

    module Clos = struct

      type t = clos

      let make env arity ty = Clos (env, arity, ty)
      let value vty = Value vty

      let arity (cty : t) : int =
        match cty with
        | Clos (_, arity, _) -> arity
        | Value _ -> 0

      let inst = inst

    end

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
      (** Local type definitions. These can be parameterised by a series of
          type arguments, corresponding to a type-lambda in System Fω:

          {@text[
            let type Foo : k₁ -> ... -> kₙ :=
              Λ a₁ ... aₙ. t
            in e
          ]}
      *)

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

      val extend_poly_ty_def : t -> Ty.name -> Ty.Clos.t -> t
      val extend_ty_def : t -> Ty.name -> Ty.Value.t -> t [@@warning "-unused-value-declaration"]
      val extend_ty_param : t -> Ty.name -> t
      val lookup_ty : t -> Ty.name -> Ty.(index * Clos.t) option

      val extend_poly_expr : t -> Expr.name -> Ty.Clos.t -> t
      val extend_expr : t -> Expr.name -> Ty.Value.t -> t
      val lookup_expr : t -> Expr.name -> Ty.Clos.t option

      val eval_ty : t -> Ty.t -> Ty.Value.t
      val quote_vty : t -> Ty.Value.t -> Ty.t
      val close_ty : t -> Ty.name list -> Ty.t -> Ty.Clos.t
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
        ty_names : Ty.name Ty.Env.t;
        ty_defs : Ty.Clos.t Ty.Env.t;
        expr_tys : (Expr.name * Ty.Clos.t) list;
      }

      let create () = {
        metas = Dynarray.create ();
        ty_size = 0;
        ty_names = Ty.Env.empty;
        ty_defs = Ty.Env.empty;
        expr_tys = [];
      }

      let fresh_vty (ctx : t) (desc : string) : Ty.Value.t =
        let meta = Ty.fresh_meta () in
        Dynarray.add_last ctx.metas (meta, desc);
        Ty.Value.Meta meta

      let extend_poly_ty_def (ctx : t) (name : Ty.name) (ty_def : Ty.Clos.t) : t =
        { ctx with
          ty_size = 1 + ctx.ty_size;
          ty_names = Ty.Env.extend name ctx.ty_names;
          ty_defs = Ty.Env.extend ty_def ctx.ty_defs;
        }

      let extend_ty_def (ctx : t) (name : Ty.name) (ty_def : Ty.Value.t) : t =
        extend_poly_ty_def ctx name (Ty.Clos.value ty_def)

      let extend_ty_param (ctx : t) (name : Ty.name) : t =
        extend_ty_def ctx name (Ty.Value.Var ctx.ty_size)

      let lookup_ty (ctx : t) (name : Ty.name) : Ty.(index * Clos.t) option =
        Ty.Env.find (String.equal name) ctx.ty_names |> Option.map @@ fun index ->
          index, Ty.Env.lookup index ctx.ty_defs

      let extend_poly_expr (ctx : t) (name : Expr.name) (cty : Ty.Clos.t) : t =
        { ctx with expr_tys = (name, cty) :: ctx.expr_tys }

      let extend_expr (ctx : t) (name : Expr.name) (ty : Ty.Value.t) : t =
        extend_poly_expr ctx name (Ty.Clos.value ty)

      let lookup_expr (ctx : t) (name : Expr.name) : Ty.Clos.t option =
        List.assoc_opt name ctx.expr_tys

      let eval_ty (ctx : t) (ty : Ty.t) : Ty.Value.t =
        Ty.eval ctx.ty_defs ty

      let quote_vty (ctx : t) (vty : Ty.Value.t) : Ty.t =
        Ty.quote ctx.ty_size vty

      let close_ty (ctx : t) (ty_params : Ty.name list) (ty : Ty.t) : Ty.Clos.t =
        Ty.Clos.make ctx.ty_defs (List.length ty_params) ty

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
          let arity, ty =
            match Ctx.lookup_ty ctx name with
            | Some (index, cty) -> Core.Ty.(Clos.arity cty, fun ty_args -> Var (index, ty_args))
            | None when name = "Bool" -> Core.Ty.(0, fun _ -> Bool)
            | None when name = "Int" -> Core.Ty.(0, fun _ -> Int)
            | None -> error "unbound type variable `%s`" name
          in

          if arity = List.length ty_args then
            ty (List.map (check_ty ctx) ty_args)
          else
            error "expected %i type %s, found %i"
              arity
              (if arity = 1 then "argument" else "arguments")
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
          let body = check_expr (Ctx.extend_expr ctx name param_vty) body body_vty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.Fun (name, Some param_ty, body), Core.Ty.Value.Fun (param_vty', body_vty) ->
          let param_ty = check_ty ctx param_ty in
          let param_vty = Ctx.eval_ty ctx param_ty in
          unify_vtys ctx ~found:param_vty ~expected:param_vty';
          let body = check_expr (Ctx.extend_expr ctx name param_vty') body body_vty in
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
          let expr, cty =
            match Ctx.lookup_expr ctx name with
            | Some cty -> Core.((fun ty_args -> Expr.Var (name, ty_args)), cty)
            | None when name = "true" -> Core.((fun _ -> Expr.Bool_lit true), Ty.Clos.value Ty.Value.Bool)
            | None when name = "false" -> Core.((fun _ -> Expr.Bool_lit false), Ty.Clos.value Ty.Value.Bool)
            | None -> error "unbound variable `%s`" name
          in

          begin match cty, ty_args with
          | cty, [] ->
              let ty_args = List.init (Core.Ty.Clos.arity cty) (fun _ -> Ctx.fresh_vty ctx "type argument") in
              expr (List.map (Ctx.quote_vty ctx) ty_args), Core.Ty.Clos.inst cty ty_args

          | cty, ty_args when Core.Ty.Clos.arity cty = List.length ty_args ->
              let ty_args = List.map (check_ty ctx) ty_args in
              expr ty_args, Core.Ty.Clos.inst cty (List.map (Ctx.eval_ty ctx) ty_args)

          | cty, ty_args ->
              error "expected %i type %s, found %i"
                (Core.Ty.Clos.arity cty)
                (if Core.Ty.Clos.arity cty = 1 then "argument" else "arguments")
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
          let body, body_vty = infer_expr (Ctx.extend_expr ctx name param_vty) body in
          Core.Expr.Fun_lit (name, Ctx.quote_vty ctx param_vty, body),
          Core.Ty.Value.Fun (param_vty, body_vty)

      | Expr.Fun (name, Some param_ty, body) ->
          let param_ty = check_ty ctx param_ty in
          let param_vty = Ctx.eval_ty ctx param_ty in
          let body, body_vty = infer_expr (Ctx.extend_expr ctx name param_vty) body in
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

    (** Check that the type parameter names are unique *)
    and check_ty_params (ty_params : string list) =
      ignore @@ ListLabels.fold_left ty_params ~init:[]
        ~f:(fun seen name ->
          if not (List.mem name seen) then name :: seen else
            error "reused type parameter name %s" name)

    (** Elaborate a polymorphic definition with an optional type annotation *)
    and infer_def (ctx : Ctx.t) (name, ty_params, def_ty, def : Expr.def) : Ctx.t * Core.Expr.def =
      check_ty_params ty_params;
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
      Ctx.extend_poly_expr ctx name (Ctx.close_ty ctx ty_params def_ty),
      (name, ty_params, def_ty, def)

    and infer_ty_def (ctx : Ctx.t) (name, ty_params, ty : Ty.def) : Ctx.t * Core.Ty.def =
      check_ty_params ty_params;
      let ty = check_ty (List.fold_left Ctx.extend_ty_param ctx ty_params) ty in
      Ctx.extend_poly_ty_def ctx name (Ctx.close_ty ctx ty_params ty),
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

  let run_tests (type a) (prog : (string -> (unit -> unit) -> unit) -> unit) : a =
    let success_count = ref 0 in
    let error_count = ref 0 in

    let run_test (name : string) (prog : unit -> unit) : unit =
      Printf.printf "test %s ... " name;

      match prog () with
      | () ->
          Printf.printf "ok\n";
          incr success_count
      | exception e ->
          Printf.printf "error:\n\n";
          Printf.printf "  %s\n\n" (Printexc.to_string e);
          String.split_on_char '\n' (Printexc.get_backtrace()) |> List.iter begin fun line ->
            Printf.printf "  %s\n" line;
          end;
          incr error_count
    in

    Printf.printf "Running tests in %s:\n\n" __FILE__;
    prog run_test;
    Printf.printf "\n";

    if !error_count <= 0 then begin
      Printf.printf "Ran %i successful tests\n\n" !success_count;
      exit 0
    end else begin
      Printf.printf "Failed %i out of %i tests\n\n" !error_count (!success_count + !error_count);
      exit 1
    end
  in

  let open Surface in

  let ( $ ) f x = Expr.App (f, x) in

  let expect_ok result =
    match result with
    | Ok x -> x
    | Error msg -> failwith msg
  in

  begin run_tests @@ fun test ->

    begin test "polymorphic identity function" @@ fun () ->

      let expr =
        Expr.Let (("id", ["A"], None,
          Fun ("x", Some (Name ("A", [])), Name ("x", []))),
          Name ("id", []) $ Tuple [])
      in

      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let (("id", ["A"], Ty.Fun (Var (0, []), Var (0, [])),
          Fun_lit ("x", Ty.Var (0, []), Var ("x", []))),
          Fun_app (Var ("id", [Ty.Tuple []]), Tuple_lit []))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);

    end;

    begin test "explicit type application" @@ fun () ->

      let expr =
        Expr.Let (("id", ["A"], None,
          Fun ("x", Some (Name ("A", [])), Name ("x", []))),
          Name ("id", [Ty.Tuple []]) $ Tuple [])
      in

      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let (("id", ["A"], Ty.Fun (Var (0, []), Var (0, [])),
          Fun_lit ("x", Ty.Var (0, []), Var ("x", []))),
          Fun_app (Var ("id", [Ty.Tuple []]), Tuple_lit []))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);

    end;

    begin test "constant function" @@ fun () ->

      let expr =
        Expr.Let (("id", ["A"], None,
          Fun ("x", Some (Name ("A", [])), Name ("x", []))),
          Let (("const", ["A"; "B"], None,
            Fun ("x", Some (Name ("A", [])), Fun ("y", Some (Name ("B", [])), Name ("x", [])))),
            Name ("const", []) $ Tuple [] $ (Name ("id", []) $ Name ("true", []))))
      in

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

    begin test "locally polymorphic definitions" @@ fun () ->

      let expr =
        Expr.Let (("kite", ["A"; "B"], Some (Fun (Name ("A", []), Fun (Name ("B", []), Name ("B", [])))),
          Fun ("x", None,
            Let (("id", ["A"], None,
              Fun ("x", Some (Name ("A", [])), Name ("x", []))),
              Name ("id", [])))),
          Name ("kite", []) $ Tuple [] $ Name ("true", []))
      in

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

    begin test "local type definitions" @@ fun () ->

      let expr =
        (* False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html *)
        Expr.Let_type (("Foo", [], Name ("Int", [])),
          Ann (Int 42, Name ("Foo", [])))
      in

      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let_type (("Foo", [], Int),
          Int_lit 42)
      ));
      assert (ty = Core.Ty.Int);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Int_lit 42);

    end;

    begin test "parameterised type definitions" @@ fun () ->

      let expr =
        (* False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html *)
        Expr.Let_type (("Id", ["A"], Name ("A", [])),
          Let (("x", [], Some (Name ("Id", [Name ("Int", [])])), Int 42),
          Name ("x", [])))
      in

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

end
