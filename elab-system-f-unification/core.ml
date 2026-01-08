(** {0 Core language} *)

(** {1 Names} *)

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
type 'a env = 'a list


(** {1 Base data types} *)

(** Unlike in previous projects, mutual recursion now happens between the syntax
    and the semantic domain due to the presence of metavariables, which means
    that it would be difficult to define {!vty} inside the {!Semantics} module.
    For simplicity we’ve chosen to inline the semantic domain into the top-level
    module, but other options could be explored in a full-scale implementation.
*)

(** {2 Types} *)

(** Identifier used for pretty printing metavariables. *)
type meta_id = int

(** Type syntax *)
type ty =
  | Local_var of index              (* Local type variables bound in the type environment (i.e. bound or rigid variables) *)
  | Meta_var of meta                (* Meta variables *)
  | Forall_type of name * ty        (* Type of a forall (i.e. the type of a term parameterised by a type) *)
  | Fun_type of ty * ty             (* Type of function types *)
  | Int_type
  | Bool_type

(** Types in weak head normal form (i.e. type values) *)
and vty =
  | Local_var of level              (* A fresh variable (used when evaluating under a binder) *)
  | Meta_var of meta                (* Meta variables *)
  | Forall_type of name * (vty -> vty)
  | Fun_type of vty * vty
  | Int_type
  | Bool_type

(** The current state of a metavariable.

    Because we have binders like {!Forall_lit} in our term syntax, the main
    complication we have in this system is preventing bound type variables
    (i.e. {!Local_var}) from escaping their scope during unification. To do this
    we use {i de Bruijn levels}, with an approach inspired by Mark Barbone’s
    {{: https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7}
    implementation} of higher rank types (See the README of this project for
    more resources).

    Conceptually, metavariables are interspersed with normal bound variables in
    the type environment. For performance reasons we don’t actually store them
    this way in our implementation, but we still need to keep track of their
    position to avoid scoping errors. This level could also be used in the
    future as an {{: https://okmij.org/ftp/ML/generalization.html} efficient way
    to implement generalisation}.
*)
and meta_state =
  | Unsolved of { id : meta_id; ty_level : level }
  (** An unsolved metavariable.

      The [ty_level] field is a constraint that represents the point in the type
      environment that we currently think the meta should be inserted (we raise
      this as needed during unification).
  *)

  | Solved of vty
  (** A metavariable with a solution, that will no-longer be updated. *)

(** Mutable representation of metavariables. These are updated in-place during
    unification and when types are forced. Alternatively we could have
    chosen to store these in a separate metacontext, like in the
    elaboration-zoo. *)
and meta = meta_state ref


(** {2 Terms} *)

(** Term syntax *)
type tm =
  | Local_var of index              (* Local term variables bound in the term environment *)
  | Let of name * ty * tm * tm      (* Local let bindings, introducing a term binding *)
  | Forall_lit of name * tm         (* Type function literal, introducing a type binding (i.e. a big-lambda abstraction) *)
  | Forall_app of tm * ty           (* Type function application *)
  | Fun_lit of name * ty * tm       (* Function literal, introducing a term binding (i.e. a lambda abstraction) *)
  | Fun_app of tm * tm              (* Function application *)
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list

(** Terms in weak head normal form (i.e. values) *)
type vtm =
  | Neu of ntm
  | Forall_lit of name * (vty -> vtm)
  | Fun_lit of name * vty * (vtm -> vtm)
  | Int_lit of int
  | Bool_lit of bool

(** Neutral values that could not be reduced to a normal form as a result of
    being stuck on something else that would not reduce further.

    For simple (non-dependent) type systems these are not actually required,
    however they allow us to {!quote} terms back to syntax, which is useful
    for pretty printing under binders.
*)
and ntm =
  | Local_var of level              (* A fresh variable (used when evaluating under a binder) *)
  | Forall_app of ntm * vty
  | Fun_app of ntm * vtm
  | Bool_elim of ntm * (unit -> vtm) * (unit -> vtm)
  | Prim_app of Prim.t * vtm list


(** Semantics of the core language *)
module Semantics = struct

  (** {1 Eliminators} *)

  let forall_app (head : vtm) (arg : vty) : vtm =
    match head with
    | Neu ntm -> Neu (Forall_app (ntm, arg))
    | Forall_lit (_, body) -> body arg
    | _ -> invalid_arg "expected type function"

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim (head : vtm) (vtm1 : unit -> vtm) (vtm2 : unit -> vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> vtm1 ()
    | Bool_lit false -> vtm2 ()
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (Int.equal t1 t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.add t1 t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.sub t1 t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.mul t1 t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (Int.neg t1)


  (** {1 Evaluation} *)

  (** Evaluate a type from the syntax into its semantic interpretation *)
  let rec eval_ty (ty_env : vty env) (ty : ty) : vty =
    match ty with
    | Local_var ty_index -> List.nth ty_env ty_index
    | Meta_var m -> Meta_var m
    | Forall_type (name, body_ty) ->
        Forall_type (name, fun arg -> eval_ty (arg :: ty_env) body_ty)
    | Fun_type (param_ty, body_ty) ->
        let param_vty = eval_ty ty_env param_ty in
        let body_vty = eval_ty ty_env body_ty in
        Fun_type (param_vty, body_vty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval_tm (ty_env : vty env) (tm_env : vtm env) (tm : tm) : vtm =
    match tm with
    | Local_var tm_index -> List.nth tm_env tm_index
    | Let (_, _, def, body) ->
        let def = eval_tm ty_env tm_env def in
        eval_tm ty_env (def :: tm_env) body
    | Forall_lit (name, body) ->
        Forall_lit (name, fun arg -> eval_tm (arg :: ty_env) tm_env body)
    | Forall_app (head, arg) ->
        let head = eval_tm ty_env tm_env head in
        let arg = eval_ty ty_env arg in
        forall_app head arg
    | Fun_lit (name, param_ty, body) ->
        let param_vty = eval_ty ty_env param_ty in
        Fun_lit (name, param_vty, fun arg -> eval_tm ty_env (arg :: tm_env) body)
    | Fun_app (head, arg) ->
        let head = eval_tm ty_env tm_env head in
        let arg = eval_tm ty_env tm_env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        let head = eval_tm ty_env tm_env head in
        let vtm1 () = eval_tm ty_env tm_env tm1 in
        let vtm2 () = eval_tm ty_env tm_env tm2 in
        bool_elim head vtm1 vtm2
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval_tm ty_env tm_env) args)


  (** {1 Quotation} *)

  (** Convert types from the semantic domain back into syntax. *)
  let rec quote_vty  (ty_size : level) (vty : vty) : ty =
    match vty with
    | Local_var ty_level -> Local_var (level_to_index ty_size ty_level)
    | Meta_var id -> Meta_var id
    | Forall_type (name, body_vty) ->
        let body = quote_vty (ty_size + 1) (body_vty (Local_var ty_size)) in
        Forall_type (name, body)
    | Fun_type (param_vty, body_vty) ->
        let param_ty = quote_vty ty_size param_vty in
        let body_ty = quote_vty ty_size body_vty in
        Fun_type (param_ty, body_ty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote_vtm (ty_size : level) (tm_size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm ty_size tm_size ntm
    | Forall_lit (name, body) ->
        let body = quote_vtm (ty_size + 1) tm_size (body (Local_var ty_size)) in
        Forall_lit (name, body)
    | Fun_lit (name, param_vty, body) ->
        let param_ty = quote_vty ty_size param_vty in
        let body = quote_vtm ty_size (tm_size + 1) (body (Neu (Local_var tm_size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_ntm (ty_size : level) (tm_size : level) (ntm : ntm) : tm =
    match ntm with
    | Local_var tm_level ->
        Local_var (level_to_index tm_size tm_level)
    | Forall_app (head, arg) ->
        Forall_app (quote_ntm ty_size tm_size head, quote_vty ty_size arg)
    | Fun_app (head, arg) ->
        Fun_app (quote_ntm ty_size tm_size head, quote_vtm ty_size tm_size arg)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm1 = quote_vtm ty_size tm_size (vtm1 ()) in
        let tm2 = quote_vtm ty_size tm_size (vtm2 ()) in
        Bool_elim (quote_ntm ty_size tm_size head, tm1, tm2)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote_vtm ty_size tm_size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise_tm (ty_env : vty env) (tm_env : vtm list) (tm : tm) : tm =
    quote_vtm (List.length ty_env) (List.length tm_env) (eval_tm ty_env tm_env tm)

end


(** {1 Functions related to metavariables} *)

(** Create a fresh, unsolved metavariable at a given level in the type environment *)
let fresh_meta : level -> meta =
  let next_id = ref 0 in
  fun ty_level ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved { id; ty_level })

(** Force any solved meta variables on the outermost part of a type. Chains of
    meta variables will be collapsed to make forcing faster in the future. This
    is sometimes referred to as {i path compression}. *)
let rec force_vty (vty : vty) : vty =
  match vty with
  | Meta_var ({ contents = Solved vty } as m) ->
      let vty = force_vty vty in
      m := Solved vty;
      vty
  | vty -> vty


(** {1 Unification} *)

exception Mismatched_types of vty * vty
exception Infinite_type of meta
exception Escaping_scope of meta * vty

(** Validate an unsolved metavariable against a solution candidate type:

    - Occurs check:
        This ensures that the candidate type does not refer to the
        to-be-solved metavariable (this would result in infinite loops).

    - Scope check:
        This ensures that the candidate type does not refer to local type
        variables that are “to-the-right” of to-be-solved metavariable in
        the type environment.

    - Level raising:
        This raises the levels of unsolved metavariables in the candidate type
        to match the level of the to-be-solved metavariable.
*)
let validate_meta_solution (ty_size : level) (m, ty_level : meta * level) (vty : vty) : unit =
  (* Traverse the solution candidate type, using the size of the typing
      context to generate free variables under binders. *)
  let rec go ty_size' (vty : vty) =
    match vty with
    | Fun_type (param_ty, body_ty) ->
        go ty_size' param_ty;
        go ty_size' body_ty
    | Forall_type (_, body_ty) ->
        go (ty_size' + 1) (body_ty (Local_var ty_size'))
    | Int_type -> ()
    | Bool_type -> ()
    | Local_var ty_level' ->
        (* Scope check *)
        if ty_level <= ty_level' && ty_level' < ty_size then
          (* Throw an error if a to-be-solved metavariable would depend on type
              variables to “the right” of it in the context. *)
          raise (Escaping_scope (m, vty))
    (* Occurs check *)
    | Meta_var m' when m == m' ->
        raise (Infinite_type m)
    (* Level raising *)
    | Meta_var ({ contents = Unsolved other } as m') ->
        (* Raise the level constraint on metavariables to match the level
          of the to-be-solved metavariable. *)
        if ty_level < other.ty_level then
          m' := Unsolved { other with ty_level }
    | Meta_var { contents = Solved vty } ->
        go ty_size' vty
  in
  go ty_size vty

(** Checks that two types (in weak-head normal form) compute to the same type
    in normal form, while updating metavariables destructively. This could be
    implemented by quoting both types and checking the resulting normal forms
    for alpha-equivalence, but it’s faster to compare the types in WHNF
    directly. *)
let rec unify_vtys (ty_size : level) (vty1 : vty) (vty2 : vty) : unit =
  match force_vty vty1, force_vty vty2 with
  | Local_var level1, Local_var level2 when level1 = level2 -> ()
  (* Use pointer equality to compare metavariables *)
  | Meta_var m1, Meta_var m2 when m1 == m2 -> ()
  | Meta_var m, vty | vty, Meta_var m ->
      begin match !m with
      | Solved vty' -> unify_vtys ty_size vty vty'
      | Unsolved { ty_level; _ } ->
          validate_meta_solution ty_size (m, ty_level) vty;
          m := Solved vty
      end
  | Forall_type (_, body_ty1), Forall_type (_, body_ty2) ->
      let x : vty = Local_var ty_size in
      unify_vtys (ty_size + 1) (body_ty1 x) (body_ty2 x)
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      unify_vtys ty_size param_ty1 param_ty2;
      unify_vtys ty_size body_ty1 body_ty2
  | Int_type, Int_type -> ()
  | Bool_type, Bool_type -> ()
  | vty1, vty2 ->
      raise (Mismatched_types (vty1, vty2))


(** {1 Pretty printing} *)

let pp_name (name : name) (ppf : Format.formatter) : unit =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_ty : name env -> ty -> Format.formatter -> unit =
  let rec pp_ty ty_names ty ppf =
    match ty with
    | Meta_var m -> pp_meta pp_ty ty_names m ppf
    | Forall_type (name, body_ty) ->
        Format.fprintf ppf "[%t] -> %t"
          (pp_name name)
          (pp_ty (name :: ty_names) body_ty)
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty ty_names param_ty)
          (pp_ty ty_names body_ty)
    | ty ->
        pp_atomic_ty ty_names ty ppf
  and pp_atomic_ty ty_names ty ppf =
    match ty with
    | Local_var ty_index -> Format.fprintf ppf "%t" (pp_name (List.nth ty_names ty_index))
    | Meta_var m -> pp_meta pp_atomic_ty ty_names m ppf
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Forall_type _ | Fun_type _ as ty ->
        Format.fprintf ppf "@[(%t)@]" (pp_ty ty_names ty)
  and pp_meta pp_ty ty_names m ppf =
    match !m with
    | Solved vty -> pp_ty ty_names (Semantics.quote_vty (List.length ty_names) vty) ppf
    | Unsolved { id; _ } -> Format.fprintf ppf "?%i" id
  in
  pp_ty

let pp_name_ann (ty_names : name env) (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (pp_ty ty_names ty)

let pp_param (ty_names : name env) (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]" (pp_name name) (pp_ty ty_names ty)

let pp_tm : name env -> name env -> tm -> Format.formatter -> unit =
  let rec pp_tm ty_names tm_names tm ppf =
    let rec go_params ty_names tm_names (tm : tm) ppf =
      match tm with
      | Forall_lit (name, body) ->
          Format.fprintf ppf "@ @[fun@ [%t]@ =>@]%t"
            (pp_name name)
            (go_params (name :: ty_names) tm_names body)
      | Fun_lit (name, param_ty, body) ->
          Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
            (pp_param ty_names name param_ty)
            (go_params ty_names (name :: tm_names) body)
      | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm ty_names tm_names tm)
    in
    match tm with
    | Let _ as tm ->
        let rec go tm_names (tm : tm) ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann ty_names name def_ty)
                (pp_tm ty_names tm_names def)
                (go (name :: tm_names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm ty_names tm_names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go tm_names tm)
    | Forall_lit (name, body) ->
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ [%t]@ =>@]%t"
          (pp_name name)
          (go_params (name :: ty_names) tm_names body)
    | Fun_lit (name, param_ty, body) ->
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
          (pp_param ty_names name param_ty)
          (go_params ty_names (name :: tm_names) body)
    | Bool_elim (head, tm1, tm2) ->
        Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_tm ty_names tm_names head)
          (pp_app_tm ty_names tm_names tm1)
          (pp_tm ty_names tm_names tm2)
    | tm ->
        pp_app_tm ty_names tm_names tm ppf
  and pp_app_tm ty_names tm_names tm ppf =
    let rec go (tm : tm) ppf =
      match tm with
      | Forall_app (head, arg) ->
          Format.fprintf ppf "%t@ [%t]"
            (go head)
            (pp_ty ty_names arg)
      | Fun_app (head, arg) ->
          Format.fprintf ppf "%t@ %t"
            (go head)
            (pp_atomic_tm ty_names tm_names arg)
      | Prim_app (prim, args) ->
          let pp_sep ppf () = Format.fprintf ppf "@ " in
          Format.fprintf ppf "#%s@ %t"
            (Prim.name prim)
            (Fun.flip (Format.pp_print_list ~pp_sep (Fun.flip (pp_atomic_tm ty_names tm_names))) args)
      | tm ->
          pp_atomic_tm ty_names tm_names tm ppf
    in
    match tm with
    | Forall_app _ | Fun_app _ | Prim_app _ as tm ->
        Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
    | tm ->
        pp_atomic_tm ty_names tm_names tm ppf
  and pp_atomic_tm ty_names tm_names tm ppf =
    match tm with
    | Local_var tm_index -> Format.fprintf ppf "%t" (pp_name (List.nth tm_names tm_index))
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Let _ | Forall_lit _ | Forall_app _ | Fun_lit _ | Fun_app _
    | Bool_elim _ | Prim_app _ as tm ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm ty_names tm_names tm)
  in
  pp_tm
