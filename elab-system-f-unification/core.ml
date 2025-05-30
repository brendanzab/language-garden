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

(** Identifier used to distinguish metavariables in in pretty printed types. *)
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
    the context. For performance reasons we don’t actually store them this way
    in our implementation, but we still need to keep track of their position to
    avoid scoping errors. This level could also be used in the future as an
    {{: https://okmij.org/ftp/ML/generalization.html} efficient way to implement
    generalisation}.
*)
and meta_state =
  | Unsolved of { id : meta_id; level : level }
  (** An unsolved metavariable.

      The [level] field is a constraint that represents the point in the context
      that we currently think the meta should be inserted (we raise this as
      needed during unification).
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
  | Bool_elim of ntm * vtm Lazy.t * vtm Lazy.t
  | Prim_app of Prim.t * vtm list


(** {2 Environment entries} *)

(** Type and term declarations *)
type decl =
  | Ty_decl
  | Tm_decl of vty

(** Type and term definitions *)
type defn =
  | Ty_defn of vty
  | Tm_defn of vtm


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

  let bool_elim (head : vtm) (vtm0 : vtm Lazy.t) (vtm1 : vtm Lazy.t) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm0, vtm1))
    | Bool_lit true -> Lazy.force vtm0
    | Bool_lit false -> Lazy.force vtm1
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (t1 = t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (t1 = t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 + t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 - t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 * t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (-t1)


  (** {1 Evaluation} *)

  (** Evaluate a type from the syntax into its semantic interpretation *)
  let rec eval_ty (env : defn env) (ty : ty) : vty =
    match ty with
    | Local_var index ->
        begin match List.nth env index with
        | Ty_defn vty -> vty
        | Tm_defn _ -> invalid_arg "expected type"
        end
    | Meta_var m -> Meta_var m
    | Forall_type (name, body_ty) ->
        Forall_type (name, fun arg -> eval_ty (Ty_defn arg :: env) body_ty)
    | Fun_type (param_ty, body_ty) ->
        let param_vty = eval_ty env param_ty in
        let body_vty = eval_ty env body_ty in
        Fun_type (param_vty, body_vty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval_tm (env : defn env) (tm : tm) : vtm =
    match tm with
    | Local_var index ->
        begin match List.nth env index with
        | Ty_defn _ -> invalid_arg "expected term"
        | Tm_defn vtm -> vtm
        end
    | Let (_, _, def, body) ->
        let def = eval_tm env def in
        eval_tm (Tm_defn def :: env) body
    | Forall_lit (name, body) ->
        Forall_lit (name, fun arg -> eval_tm (Ty_defn arg :: env) body)
    | Forall_app (head, arg) ->
        let head = eval_tm env head in
        let arg = eval_ty env arg in
        forall_app head arg
    | Fun_lit (name, param_ty, body) ->
        let param_vty = eval_ty env param_ty in
        Fun_lit (name, param_vty, fun arg -> eval_tm (Tm_defn arg :: env) body)
    | Fun_app (head, arg) ->
        let head = eval_tm env head in
        let arg = eval_tm env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm0, tm1) ->
        let head = eval_tm env head in
        let vtm0 = lazy (eval_tm env tm0) in
        let vtm1 = lazy (eval_tm env tm1) in
        bool_elim head vtm0 vtm1
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval_tm env) args)


  (** {1 Quotation} *)

  (** Convert types from the semantic domain back into syntax. *)
  let rec quote_vty  (size : level) (vty : vty) : ty =
    match vty with
    | Local_var level -> Local_var (level_to_index size level)
    | Meta_var id -> Meta_var id
    | Forall_type (name, body_vty) ->
        let body = quote_vty (size + 1) (body_vty (Local_var size)) in
        Forall_type (name, body)
    | Fun_type (param_vty, body_vty) ->
        let param_ty = quote_vty size param_vty in
        let body_ty = quote_vty size body_vty in
        Fun_type (param_ty, body_ty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote_vtm (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm size ntm
    | Forall_lit (name, body) ->
        let body = quote_vtm (size + 1) (body (Local_var size)) in
        Forall_lit (name, body)
    | Fun_lit (name, param_vty, body) ->
        let param_ty = quote_vty size param_vty in
        let body = quote_vtm (size + 1) (body (Neu (Local_var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_ntm (size : level) (ntm : ntm) : tm =
    match ntm with
    | Local_var level ->
        Local_var (level_to_index size level)
    | Forall_app (head, arg) ->
        Forall_app (quote_ntm size head, quote_vty size arg)
    | Fun_app (head, arg) ->
        Fun_app (quote_ntm size head, quote_vtm size arg)
    | Bool_elim (head, vtm0, vtm1) ->
        let tm0 = quote_vtm size (Lazy.force vtm0) in
        let tm1 = quote_vtm size (Lazy.force vtm1) in
        Bool_elim (quote_ntm size head, tm0, tm1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote_vtm size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise_tm (env : defn env) (tm : tm) : tm =
    quote_vtm (List.length env) (eval_tm env tm)

end


(** {1 Functions related to metavariables} *)

(** Create a fresh, unsolved metavariable at a given level in the context *)
let fresh_meta : level -> meta =
  let next_id = ref 0 in
  fun level ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved { id; level })

(** Force any solved meta variables on the outermost part of a type. Chains of
    meta variables will be collapsed to make forcing faster in the future. This
    is sometimes referred to as {i path compression}. *)
let rec force_vty (vty : vty) : vty =
  match vty with
  | Meta_var m as vty ->
      begin match !m with
      | Solved vty ->
          let vty = force_vty vty in
          m := Solved vty;
          vty
      | Unsolved _ -> vty
      end
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
        the context.

    - Level raising:
        This raises the levels of unsolved metavariables in the candidate type
        to match the level of the to-be-solved metavariable.
*)
let validate_meta_solution (size : level) (m, level : meta * level) (vty : vty) : unit =
  (** Traverse the solution candidate type, using the size of the typing
      context to generate free variables under binders. *)
  let rec go size' (vty : vty) =
    match vty with
    | Fun_type (param_ty, body_ty) ->
        go size' param_ty;
        go size' body_ty
    | Forall_type (_, body_ty) ->
        go (size' + 1) (body_ty (Local_var size'))
    | Int_type -> ()
    | Bool_type -> ()
    | Local_var level' ->
        (* Scope check *)
        if level <= level' && level' < size then
          (* Throw an error if a to-be-solved metavariable would depend on type
              variables to “the right” of it in the context. *)
          raise (Escaping_scope (m, vty))
    | Meta_var m' ->
        (* Occurs check *)
        if m == m' then
          raise (Infinite_type m);
        (* Level raising *)
        begin match !m' with
        | Unsolved { id = id'; level = level' } ->
            (* Raise the level constraint on metavariables to match the level
              of the to-be-solved metavariable. *)
            if level < level' then
              m' := Unsolved { id = id'; level }
        | Solved vty ->
            go size' vty
        end
  in
  go size vty

let rec unify_vtys (size : level) (vty1 : vty) (vty2 : vty) : unit =
  match force_vty vty1, force_vty vty2 with
  | Local_var level1, Local_var level2 when level1 = level2 -> ()
  (* Use pointer equality to compare metavariables *)
  | Meta_var m1, Meta_var m2 when m1 == m2 -> ()
  | Meta_var m, vty | vty, Meta_var m ->
      begin match !m with
      | Solved vty' -> unify_vtys size vty vty'
      | Unsolved { level; _ } ->
          validate_meta_solution size (m, level) vty;
          m := Solved vty
      end
  | Forall_type (_, body_ty1), Forall_type (_, body_ty2) ->
      let x : vty = Local_var size in
      unify_vtys (size + 1) (body_ty1 x) (body_ty2 x)
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      unify_vtys size param_ty1 param_ty2;
      unify_vtys size body_ty1 body_ty2
  | Int_type, Int_type -> ()
  | Bool_type, Bool_type -> ()
  | vty1, vty2 ->
      raise (Mismatched_types (vty1, vty2))


(** {1 Zonking} *)

(** These functions flatten solved metavariables in types. This is important
    for pretty printing types, as we want to be able to ‘see through’
    metavariables to properly associate function types.

    Resources:

    - https://stackoverflow.com/questions/31889048/what-does-the-ghc-source-mean-by-zonk
    - https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-August/024209.html
*)

(* Deeply force a type, leaving only unsolved metavariables remaining *)
let rec zonk_ty (size : level) (ty : ty) : ty =
  match ty with
  | Meta_var m ->
      begin match !m with
      | Unsolved _ -> Meta_var m
      | Solved vty -> zonk_ty size (Semantics.quote_vty size vty)
      end
  | Local_var index -> Local_var index
  | Forall_type (name, body_ty) ->
      Forall_type (name, zonk_ty (size + 1) body_ty)
  | Fun_type (param_ty, body_ty) ->
      Fun_type (zonk_ty size param_ty, zonk_ty size body_ty)
  | Int_type -> Int_type
  | Bool_type -> Bool_type

(** Force all metavariables in a term *)
let rec zonk_tm (size : level) (tm : tm) : tm =
  match tm with
  | Local_var index -> Local_var index
  | Let (name, def_ty, def, body) ->
      Let (name, zonk_ty size def_ty, zonk_tm size def, zonk_tm (size + 1) body)
  | Forall_lit (name, body) ->
      Forall_lit (name, zonk_tm (size + 1) body)
  | Forall_app (head, arg) ->
      Forall_app (zonk_tm size head, zonk_ty size arg)
  | Fun_lit (name, param_ty, body) ->
      Fun_lit (name, zonk_ty size param_ty, zonk_tm (size + 1) body)
  | Fun_app (head, arg) ->
      Fun_app (zonk_tm size head, zonk_tm size arg)
  | Int_lit i -> Int_lit i
  | Bool_lit b -> Bool_lit b
  | Bool_elim (head, tm1, tm2) ->
      Bool_elim (zonk_tm size head, zonk_tm size tm1, zonk_tm size tm2)
  | Prim_app (prim, args) ->
      Prim_app (prim, List.map (zonk_tm size) args)


(** {1 Pretty printing} *)

let pp_name fmt name =
  match name with
  | Some name -> Format.pp_print_string fmt name
  | None -> Format.pp_print_string fmt "_"

let rec pp_ty (names : name env) (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | Forall_type (name, body_ty) ->
      Format.fprintf fmt "[%a] -> %a"
        pp_name name
        (pp_ty (name :: names)) body_ty
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        (pp_atomic_ty names) param_ty
        (pp_ty names) body_ty
  | ty ->
      pp_atomic_ty names fmt ty
and pp_atomic_ty (names : name env) (fmt : Format.formatter) (ty : ty) =
  match ty with
  | Local_var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
  | Meta_var m -> pp_meta fmt m
  | Int_type -> Format.fprintf fmt "Int"
  | Bool_type -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" (pp_ty names) ty
and pp_meta (fmt : Format.formatter) (m : meta) =
  match !m with
  | Solved _ -> failwith "expected forced meta"
  | Unsolved { id; _ } -> Format.fprintf fmt "?%i" id

let pp_name_ann names fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%a :@]@ %a@]" pp_name name (pp_ty names) ty

let pp_param names fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]" pp_name name (pp_ty names) ty

let rec pp_tm (names : name env) (fmt : Format.formatter) (tm : tm) : unit =
  let rec go_params names fmt (tm : tm) =
    match tm with
    | Forall_lit (name, body) ->
        Format.fprintf fmt "@ @[fun@ [%a]@ =>@]%a"
          pp_name name
          (go_params (name :: names)) body
    | Fun_lit (name, param_ty, body) ->
        Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
          (pp_param names) (name, param_ty)
          (go_params (name :: names)) body
    | tm -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_tm names) tm
  in
  match tm with
  | Let _ as tm ->
      let rec go names fmt (tm : tm) =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              (pp_name_ann names) (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<v>%a@]" (go names) tm
  | Forall_lit (name, body) ->
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ [%a]@ =>@]%a"
        pp_name name
        (go_params (name :: names)) body
  | Fun_lit (name, param_ty, body) ->
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        (pp_param names) (name, param_ty)
        (go_params (name :: names)) body
  | Bool_elim (head, tm0, tm1) ->
      Format.fprintf fmt "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt tm =
  match tm with
  | Forall_app (head, arg) ->
      Format.fprintf fmt "@[%a@ [%a]@]"
        (pp_app_tm names) head
        (pp_ty names) arg
  | Fun_app (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | Prim_app (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt tm =
  match tm with
  | Local_var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
