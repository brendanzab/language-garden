(** {0 Core language} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option

(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** {i De Bruijn index} that represents a variable occurrence by the number of
    binders between the occurrence and the binder it refers to. *)
type index = int

(** {i De Bruijn level} that represents a variable occurrence by the number of
    binders from the top of the environment to the binder that the occurrence
    refers to. These do not change their meaning as new bindings are added to
    the environment. *)
type level = int

(** [level_to_index size level] converts [level] to an {!index} that is bound in
    an environment of the supplied [size], where [size] represents the next
    fresh {!level} to be bound in the environment.

    Assumes that [size > level].
*)
let level_to_index (size : level) (level : level) =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
module Env : sig

  type _ t

  val empty : 'a t
  val extend : 'a -> 'a t -> 'a t
  val lookup : index -> 'a t -> 'a
  val find : ('a -> bool) -> 'a t -> index option
  val size : 'a t -> level

end = struct

  type 'a t = 'a list

  let empty = []
  let extend = List.cons
  let lookup index env = List.nth env index
  let find = List.find_index
  let size = List.length

end

(** Base datatypes describing the syntax of types. *)
module rec Ty_data : sig

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


(** Types in the core language *)
module rec Ty : sig

  include module type of Ty_data

  (** Create a fresh, unsolved metavariable *)
  val fresh_meta : unit -> meta

  (** Evaluate a type into a value *)
  val eval : Ty.Clos.t Env.t -> t -> Ty.Value.t

  (** Quote an evaluated type back into its normal form *)
  val quote : level -> Ty.Value.t -> t

  (** Pretty print a type *)
  val pp : name Env.t -> t -> Format.formatter -> unit

  (** Evaluated types. These can be used at deeper parts of the program without
      needing to be re-indexed. *)
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

  (** Types that can be instantiated with a series of type arguments.
      These are used as polymorphic types during elaboration, either for
      parameterised type definitions, or for the types of polymorphic term
      definitions. *)
  module Clos : sig

    type t

    (** Create a closure must be instantiated with a given number of types. *)
    val make : t Env.t -> int -> Ty.t -> t

    (** Create a closure that is instantiated with no types. *)
    val value : Value.t -> t

    (** The number of type arguments this closure should be instantiated with *)
    val arity : t -> int

    (** Instantiate the closure with a list of types. *)
    val inst : t -> Value.t list -> Value.t

  end

end = struct

  include Ty_data

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
        let extend_value acc arg = Env.extend (Value arg) acc in
        eval (List.fold_left extend_value env args) body
    | Value vty, [] -> vty
    | _, _ -> failwith "mismatched arity"

  let rec quote (size : level) (vty : Value.t) : t =
    match vty with
    | Value.Var level -> Var (level_to_index size level, [])
    | Value.Meta meta -> Meta meta
    | Value.Fun (param_ty, body_ty) -> Fun (quote size param_ty, quote size body_ty)
    | Value.Tuple elem_tys -> Tuple (List.map (quote size) elem_tys)
    | Value.Bool -> Bool
    | Value.Int -> Int

  let pp (names : name Env.t) (ty : t) (ppf : Format.formatter) : unit =
    let size = Env.size names in

    let pp_name (name : name) (ppf : Format.formatter) : unit =
      match name with
      | Some name -> Format.pp_print_string ppf name
      | None -> Format.pp_print_string ppf "_"
    in

    let rec pp_ty ty ppf =
      match ty with
      | Meta m -> pp_meta pp_ty m ppf
      | Fun (param_ty, body_ty) ->
          Format.fprintf ppf "%t -> %t"
            (pp_app_ty param_ty)
            (pp_ty body_ty)
      | ty ->
          pp_app_ty ty ppf
    and pp_app_ty ty ppf =
      match ty with
      | Var (index, ((_ :: _) as args)) ->
          Format.fprintf ppf "@[<hv 2>%t@ %a@]"
            (pp_name (Env.lookup index names))
            (Format.pp_print_list (Fun.flip pp_atomic_ty)
              ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ "))
            args
      | ty ->
          pp_atomic_ty ty ppf
    and pp_atomic_ty ty ppf =
      match ty with
      | Var (index, []) -> Format.fprintf ppf "%t" (pp_name (Env.lookup index names))
      | Meta m -> pp_meta pp_atomic_ty m ppf
      | Tuple [] -> Format.fprintf ppf "()"
      | Tuple [elem_ty] -> Format.fprintf ppf "@[(%t,)@]" (pp_ty elem_ty)
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


(** Terms in the core language *)
module Tm = struct

  (** Term syntax *)
  type t =
    | Var of index * Ty.t list
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

    | Let_rec of def list * t
    (** Mutually recursive bindings *)

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
    | Int_lit of int
    | Bool_lit of bool
    | Bool_elim of t * t * t
    | Prim_app of Prim.t * t list

  and def =
    name * name list * Ty.t * t

  let pp (ty_names : name Env.t) (names : name Env.t) : t -> Format.formatter -> unit =
    let pp_name (name : name) (ppf : Format.formatter) : unit =
      match name with
      | Some name -> Format.pp_print_string ppf name
      | None -> Format.pp_print_string ppf "_"
    in

    let pp_name_ann (ty_names : name Env.t) (name : name) (ty_params : name list) (ty : Ty.t) (ppf : Format.formatter) : unit =
      match ty_params with
      | [] -> Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (Ty.pp ty_names ty)
      | ty_params ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[<2>@[%t@ @[[%t]@]@ :@]@ %t@]"
            (pp_name name)
            (Fun.flip (Format.pp_print_list (Fun.flip pp_name) ~pp_sep) ty_params)
            (Ty.pp (List.fold_left (Fun.flip Env.extend) ty_names ty_params) ty)

    and pp_param (ty_names : name Env.t) (name : name) (ty : Ty.t) (ppf : Format.formatter) : unit =
      Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]"
        (pp_name name)
        (Ty.pp ty_names ty)
    in

    let rec pp_tm ty_names names tm ppf =
      match tm with
      | Let _ | Let_rec _ | Let_type _ as tm ->
          let rec go ty_names names tm ppf =
            match tm with
            | Let ((name, ty_params, def_ty, def), body) ->
                Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                  (pp_name_ann ty_names name ty_params def_ty)
                  (pp_tm (List.fold_left (Fun.flip Env.extend) ty_names ty_params) names def)
                  (go ty_names (Env.extend name names) body)
            | Let_rec (defs, body) ->
                let extend names (n, _, _, _) = Env.extend n names in
                let rec_names = List.fold_left extend names defs in
                (* FIXME: indentation *)
                Format.fprintf ppf "@[@[let@ rec@ {@]%t@ };@]@ %t"
                  (defs |> Fun.flip @@ Format.pp_print_list
                    ~pp_sep:Format.pp_print_nothing
                    (fun ppf (name, ty_params, def_ty, def) ->
                      Format.fprintf ppf "@;<1 2>@[<2>@[%t@ :=@]@ %t@];"
                        (pp_name_ann ty_names name ty_params def_ty)
                        (pp_tm (List.fold_left (Fun.flip Env.extend) ty_names ty_params) rec_names def)))
                  (go ty_names rec_names body)
            | Let_type ((ty_name, ty_params, ty_def), body) ->
                let pp_sep ppf () = Format.fprintf ppf "@ " in
                Format.fprintf ppf "@[<2>@[let type @[%t@] :=@]@ @[%t;@]@]@ %t"
                  (Fun.flip (Format.pp_print_list (Fun.flip pp_name) ~pp_sep) (ty_name :: ty_params))
                  (Ty.pp (List.fold_left (Fun.flip Env.extend) ty_names ty_params) ty_def)
                  (go (Env.extend ty_name ty_names) names body)
            | tm -> Format.fprintf ppf "@[%t@]" (pp_tm ty_names names tm)
          in
          Format.fprintf ppf "@[<v>%t@]"
            (go ty_names names tm)
      | Fun_lit (name, param_ty, body) ->
          let rec go names tm ppf =
            match tm with
            | Fun_lit (name, param_ty, body) ->
                Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
                  (pp_param ty_names name param_ty)
                  (go (Env.extend name names) body)
            | (Let _) as tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm ty_names names tm)
            | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm ty_names names tm)
          in
          (* TODO: use a vertical box if a series of functions are followed by a let *)
          Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
            (pp_param ty_names name param_ty)
            (go (Env.extend name names) body)
      | Bool_elim (head, tm1, tm2) ->
          Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
            (pp_app_tm ty_names names head)
            (pp_app_tm ty_names names tm1)
            (pp_tm ty_names names tm2)
      | tm ->
          pp_app_tm ty_names names tm ppf
    and pp_app_tm ty_names names tm ppf =
      let rec go tm ppf =
        match tm with
        | Fun_app (head, arg) ->
            Format.fprintf ppf "%t@ %t"
              (go head)
              (pp_proj_tm ty_names names arg)
        | Prim_app (prim, args) ->
            let pp_sep ppf () = Format.fprintf ppf "@ " in
            Format.fprintf ppf "#%s@ %t"
              (Prim.name prim)
              (Fun.flip (Format.pp_print_list (Fun.flip (pp_proj_tm ty_names names)) ~pp_sep) args)
        | tm ->
            pp_proj_tm ty_names names tm ppf
      in
      match tm with
      | Fun_app _ | Prim_app _ as tm ->
          Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
      | tm ->
          pp_proj_tm ty_names names tm ppf
    and pp_proj_tm ty_names names tm ppf =
      let rec go tm ppf =
        match tm with
        | Tuple_proj (head, label) ->
              Format.fprintf ppf "%t@,.%i" (go head) label
        | tm ->
            pp_atomic_tm ty_names names tm ppf
      in
      match tm with
      | Tuple_proj _ as tm ->
          Format.fprintf ppf "@[<2>%t@]" (go tm)
      | tm ->
          pp_atomic_tm ty_names names tm ppf
    and pp_atomic_tm ty_names names tm ppf =
      match tm with
      | Var (index, []) -> Format.fprintf ppf "%t" (pp_name (Env.lookup index names))
      | Var (index, ((_ :: _) as ty_args)) ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "%t@ @[[%t]@]"
            (pp_name (Env.lookup index names))
            (Fun.flip (Format.pp_print_list (Fun.flip (Ty.pp ty_names)) ~pp_sep) ty_args)
      | Tuple_lit [] -> Format.fprintf ppf "()"
      | Tuple_lit [elem] -> Format.fprintf ppf "@[(%t,)@]" (pp_tm ty_names names elem)
      | Tuple_lit elems ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[(%t)@]"
            (Fun.flip (Format.pp_print_list (Fun.flip (pp_tm ty_names names)) ~pp_sep) elems)
      | Int_lit i -> Format.fprintf ppf "%i" i
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Let _ | Let_rec _ | Let_type _ | Fun_lit _ | Fun_app _
      | Tuple_proj _ | Bool_elim _ | Prim_app _ as tm ->
          Format.fprintf ppf "@[(%t)@]" (pp_tm ty_names names tm)
    in
    pp_tm ty_names names

  module Value = struct

    type t =
      | Fun_lit of (t -> t)
      | Tuple_lit of t list
      | Int_lit of int
      | Bool_lit of bool

    let rec pp (vexpr : t) (ppf : Format.formatter) : unit =
      match vexpr with
      | Fun_lit _ -> Format.fprintf ppf "<function>"
      | Tuple_lit [] -> Format.fprintf ppf "()"
      | Tuple_lit [elem] -> Format.fprintf ppf "@[(%t)@]" (pp elem)
      | Tuple_lit elems ->
          let pp_sep ppf () = Format.fprintf ppf ",@ " in
          Format.fprintf ppf "@[(%t)@]"
            (Fun.flip (Format.pp_print_list (Fun.flip pp) ~pp_sep) elems)
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Int_lit i -> Format.fprintf ppf "%i" i

  end

  let rec eval (env : Value.t ref Env.t) (tm : t) : Value.t =
    match tm with
    | Var (index, _) -> !(Env.lookup index env)
    | Let ((_, _, _, def), body) ->
        let def = eval env def in
        eval (Env.extend (ref def) env) body
    | Let_rec (defs, body) ->
        (* Add placeholder bindings to the environment then back-patch them
           with their corresponding recursive definitions. This is inspired by
           Landin’s approach in “The Mechanical Evaluation of Expressions” and
           R⁵RS Scheme’s approach to encoding [letrec] (see Section 7.3 of the
           {{: https://dl.acm.org/doi/10.1145/290229.290234} Revised⁵ Report
           on the Algorithmic Language Scheme}).

           This relies on each definition being a function in order to avoid
           the bindings from being accessed before they have been defined. *)
        let undefined = Value.Fun_lit (fun _ -> failwith "undefined") in
        let bindings = defs |> List.map (fun _ -> ref undefined) in
        let env = List.fold_left (fun env binding -> Env.extend binding env) env bindings in
        List.iter2 (fun vdef (_, _, _, tm) -> vdef := eval env tm) bindings defs;
        eval env body
    | Let_type (_, body) ->
        eval env body
    | Fun_lit (_, _, body) ->
        Value.Fun_lit (fun arg -> eval (Env.extend (ref arg) env) body)
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
    | Int_lit i -> Value.Int_lit i
    | Bool_lit b -> Value.Bool_lit b
    | Bool_elim (head, true_body, false_body) ->
        begin match eval env head with
        | Value.Bool_lit true -> eval env true_body
        | Value.Bool_lit false -> eval env false_body
        | _ -> invalid_arg "eval"
        end
    | Prim_app (prim, args) ->
        begin match prim, List.map (eval env) args with
        | Prim.Bool_eq, Value.[Bool_lit t1; Bool_lit t2] -> Value.Bool_lit (Bool.equal t1 t2)
        | Prim.Int_eq, Value.[Int_lit t1; Int_lit t2] -> Value.Bool_lit (Int.equal t1 t2)
        | Prim.Int_add, Value.[Int_lit t1; Int_lit t2] -> Value.Int_lit (Int.add t1 t2)
        | Prim.Int_sub, Value.[Int_lit t1; Int_lit t2] -> Value.Int_lit (Int.sub t1 t2)
        | Prim.Int_mul, Value.[Int_lit t1; Int_lit t2] -> Value.Int_lit (Int.mul t1 t2)
        | Prim.Int_neg, Value.[Int_lit t1] -> Value.Int_lit (Int.neg t1)
        | _ -> invalid_arg "eval"
        end

end
