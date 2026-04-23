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

(** To illustrate the relationship between indices and levels, the level and
    index of the variable [g] can be found as follows:

    {@text[
                    level = 3      index = 1
                │─λ───λ───λ──────▶︎│◀︎─λ──────│
                │                 │         │
      Names     │ λn. λf. λx. n (λg. λh. h (g f)) (λu. x) (λu. u)
      Indices   │ λ   λ   λ   2 (λ   λ   0 (1 3)) (λ   1) (λ   0)
      Levels    │ λ   λ   λ   0 (λ   λ   4 (3 1)) (λ   2) (λ   3)
    ]}

    Nameless variable representations like {i De Bruijn indices} and {i De
    Bruijn levels} have a reputation for off-by-one errors and requiring
    expensive re-indexing operations when performing substitutions. Thankfully
    these issues can be largely avoided by choosing different variable
    representations for the syntax and the semantic domain:

    - In the {i syntax} we represent bound variables with {!index}. This allows
      us to easily compare terms for alpha equivalence and quickly look up
      bindings based on their position in the environment.
    - In the {i semantic domain} we represent free variables with {!level}.
      Because the meaning of levels remains the same as new bindings are added
      to the environment, this lets us use terms at greater binding depths
      without needing to reindex them first.

    The only time we really need to reindex terms is when quoting from the
    syntax back to the semantic domain, using the {!level_to_index} function,
    and only requires a single traversal of the term.

    This approach is documented in more detail in Chapter 3 of Andreas Abel’s
    Thesis, {{: https://www.cse.chalmers.se/~abela/habil.pdf} “Normalization
    by Evaluation: Dependent Types and Impredicativity”}. Andras Kovacs also
    explains the approach in {{: https://proofassistants.stackexchange.com/a/910/309}
    an answer on the Proof Assistants Stack Exchange}.
*)

(** [level_to_index size level] converts [level] to an {!index} that is bound in
    an environment of the supplied [size], where [size] represents the next
    fresh {!level} to be bound in the environment.

    Assumes that [size > level].
*)
let level_to_index (size : level) (level : level) : index =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
type 'a env = 'a list


module Ty = struct

  (** Type syntax *)
  type t =
    | Var of index            (* Local type variables bound in the type environment *)
    | Forall of name * t      (* Type of a forall (i.e. the type of a term parameterised by a type) *)
    | Fun of t * t            (* Type of function types *)
    | Int
    | Bool

  (** Types in weak head normal form *)
  and value =
    | Var of level
    | Forall of name * clos
    | Fun of value * value
    | Int
    | Bool

  (** Type closures *)
  and clos =
    | Clos of value env * t


  (** {2 Semantics of types} *)

  (** Evaluate a type from the syntax into its semantic interpretation *)
  let rec eval (ty_env : value env) (ty : t) : value =
    match ty with
    | Var ty_index -> List.nth ty_env ty_index
    | Forall (name, body_ty) ->
        Forall (name, Clos (ty_env, body_ty))
    | Fun (param_ty, body_ty) ->
        let param_vty = eval ty_env param_ty in
        let body_vty = eval ty_env body_ty in
        Fun (param_vty, body_vty)
    | Int -> Int
    | Bool -> Bool

  (** Instantiate a type closure withe a type argument *)
  and inst_clos (Clos (ty_env, body_ty) : clos) (arg : value) : value =
    eval (arg :: ty_env) body_ty

  (** The next variable that will bound in the type environment. *)
  let next_var (size : level) : value =
    Var size

  (** Convert types from the semantic domain back into syntax. *)
  let rec quote  (ty_size : level) (vty : value) : t =
    match vty with
    | Var ty_level -> Var (level_to_index ty_size ty_level)
    | Forall (name, body_vty) ->
        let body = quote (ty_size + 1) (inst_clos body_vty (next_var ty_size)) in
        Forall (name, body)
    | Fun (param_vty, body_vty) ->
        let param_ty = quote ty_size param_vty in
        let body_ty = quote ty_size body_vty in
        Fun (param_ty, body_ty)
    | Int -> Int
    | Bool -> Bool


  (** {1 Conversion Checking} *)

  (** Checks that two types (in weak-head normal form) compute to the same type
      in normal form. This could be implemented naively by quoting both types
      and checking the resulting normal forms for alpha-equivalence, but it’s
      faster to compare the types in WHNF directly. *)
  let rec is_convertible (ty_size : level) (vty1 : value) (vty2 : value) : bool =
    match vty1, vty2 with
    | Var ty_level1, Var ty_level2 -> ty_level1 = ty_level2
    | Forall (_, body_ty1), Forall (_, body_ty2) ->
        let x = next_var ty_size in
        is_convertible (ty_size + 1) (inst_clos body_ty1 x) (inst_clos body_ty2 x)
    | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
        is_convertible ty_size param_ty1 param_ty2
          && is_convertible ty_size body_ty1 body_ty2
    | Int, Int -> true
    | Bool, Bool -> true
    | _, _ -> false


  (** {2 Pretty printing} *)

  let pp_name (name : name) (ppf : Format.formatter) : unit =
    match name with
    | Some name -> Format.pp_print_string ppf name
    | None -> Format.pp_print_string ppf "_"

  let pp : name env -> t -> Format.formatter -> unit =
    let rec pp ty_names ty ppf =
      match ty with
      | Forall (name, body_ty) ->
          Format.fprintf ppf "[%t] -> %t"
            (pp_name name)
            (pp (name :: ty_names) body_ty)
      | Fun (param_ty, body_ty) ->
          Format.fprintf ppf "%t -> %t"
            (pp_atomic ty_names param_ty)
            (pp ty_names body_ty)
      | ty ->
          pp_atomic ty_names ty ppf
    and pp_atomic ty_names ty ppf =
      match ty with
      | Var ty_index -> Format.fprintf ppf "%t" (pp_name (List.nth ty_names ty_index))
      | Int -> Format.fprintf ppf "Int"
      | Bool -> Format.fprintf ppf "Bool"
      | Forall _ | Fun _ as ty ->
          Format.fprintf ppf "@[(%t)@]" (pp ty_names ty)
    in
    pp

end


module Tm = struct

  (** Term syntax *)
  type t =
    | Var of index              (* Local term variables bound in the term environment *)
    | Prim of Prim.t
    | Let of name * Ty.t * t * t      (* Local let bindings, introducing a term binding *)
    | Forall_lit of name * t          (* Type function literal, introducing a type binding (i.e. a big-lambda abstraction) *)
    | Forall_app of t * Ty.t          (* Type function application *)
    | Fun_lit of name * Ty.t * t      (* Function literal, introducing a term binding (i.e. a lambda abstraction) *)
    | Fun_app of t * t                (* Function application *)
    | Int_lit of int
    | Bool_lit of bool
    | Bool_elim of t * t * t

  (** Terms in weak head normal form (i.e. values) *)
  type value =
    | Neu of neu
    | Forall_lit of name * Ty.value clos
    | Fun_lit of name * Ty.value * value clos
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and neu =
    | Var of level
    | Forall_app of neu * Ty.value
    | Prim_app of Prim.t * value list
    | Fun_app of neu * value
    | Bool_elim of neu * thunk * thunk

  and 'arg clos =
    | Clos_ty : Ty.value env * value env * t -> Ty.value clos
    | Clos_tm : Ty.value env * value env * t -> value clos

  and thunk =
    | Thunk of Ty.value env * value env * t


  (** {2 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (ty_env : Ty.value env) (tm_env : value env) (tm : t) : value =
    match tm with
    | Var tm_index -> List.nth tm_env tm_index
    | Prim prim -> prim_app prim []
    | Let (_, _, def, body) ->
        let def = eval ty_env tm_env def in
        eval ty_env (def :: tm_env) body
    | Forall_lit (name, body) ->
        Forall_lit (name, Clos_ty (ty_env, tm_env, body))
    | Forall_app (head, arg) ->
        let head = eval ty_env tm_env head in
        let arg = Ty.eval ty_env arg in
        forall_app head arg
    | Fun_lit (name, param_ty, body) ->
        let param_vty = Ty.eval ty_env param_ty in
        Fun_lit (name, param_vty, Clos_tm (ty_env, tm_env, body))
    | Fun_app (head, arg) ->
        let head = eval ty_env tm_env head in
        let arg = eval ty_env tm_env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        let head = eval ty_env tm_env head in
        bool_elim head (Thunk (ty_env, tm_env, tm1)) (Thunk (ty_env, tm_env, tm2))

  (** Instantiate a closure with a value *)
  and inst_clos : type a. a clos -> a -> value =
    fun clos arg ->
      match clos with
      | Clos_ty (ty_env, tm_env, body) -> eval (arg :: ty_env) tm_env body
      | Clos_tm (ty_env, tm_env, body) -> eval ty_env (arg :: tm_env) body

  (** Force the computation of a thunk *)
  and force_thunk (Thunk (ty_env, tm_env, body) : thunk) : value =
    eval ty_env tm_env body


  (** {2 Eliminators} *)

  and forall_app (head : value) (arg : Ty.value) : value =
    match head with
    | Neu ntm -> Neu (Forall_app (ntm, arg))
    | Forall_lit (_, body) -> inst_clos body arg
    | _ -> invalid_arg "expected type function"

  and prim_app (prim : Prim.t) (args : value list) : value =
    match prim, args with
    | Bool_eq, [Bool_lit t2; Bool_lit t1] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq, [Int_lit t2; Int_lit t1] -> Bool_lit (Int.equal t1 t2)
    | Int_add, [Int_lit t2; Int_lit t1] -> Int_lit (Int.add t1 t2)
    | Int_sub, [Int_lit t2; Int_lit t1] -> Int_lit (Int.sub t1 t2)
    | Int_mul, [Int_lit t2; Int_lit t1] -> Int_lit (Int.mul t1 t2)
    | Int_neg, [Int_lit t1] -> Int_lit (Int.neg t1)
    | prim, args -> Neu (Prim_app (prim, args))

  and fun_app (head : value) (arg : value) : value =
    match head with
    | Neu (Prim_app (prim, args)) -> prim_app prim (arg :: args)
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> inst_clos body arg
    | _ -> invalid_arg "expected function"

  and bool_elim (head : value) (vtm1 : thunk) (vtm2 : thunk) : value =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> force_thunk vtm1
    | Bool_lit false -> force_thunk vtm2
    | _ -> invalid_arg "expected boolean"


  (** {2 Quotation} *)

  (** The next variable that will bound in the term environment. *)
  let next_var (tm_size : level) : value =
    Neu (Var tm_size)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (ty_size : level) (tm_size : level) (vtm : value) : t =
    match vtm with
    | Neu ntm -> quote_neu ty_size tm_size ntm
    | Forall_lit (name, body) ->
        let body = quote (ty_size + 1) tm_size (inst_clos body (Ty.next_var ty_size)) in
        Forall_lit (name, body)
    | Fun_lit (name, param_vty, body) ->
        let param_ty = Ty.quote ty_size param_vty in
        let body = quote ty_size (tm_size + 1) (inst_clos body (next_var tm_size)) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (ty_size : level) (tm_size : level) (ntm : neu) : t =
    match ntm with
    | Var tm_level ->
        Var (level_to_index tm_size tm_level)
    | Forall_app (head, arg) ->
        Forall_app (quote_neu ty_size tm_size head, Ty.quote ty_size arg)
    | Prim_app (prim, []) -> Prim prim
    | Prim_app (prim, arg :: args) ->
        let head = quote_neu ty_size tm_size (Prim_app (prim, args)) in
        let arg = quote ty_size tm_size arg in
        Fun_app (head, arg)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu ty_size tm_size head, quote ty_size tm_size arg)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm1 = quote ty_size tm_size (force_thunk vtm1) in
        let tm2 = quote ty_size tm_size (force_thunk vtm2) in
        Bool_elim (quote_neu ty_size tm_size head, tm1, tm2)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (ty_env : Ty.value env) (tm_env : value list) (tm : t) : t =
    quote (List.length ty_env) (List.length tm_env) (eval ty_env tm_env tm)


  (** {2 Pretty printing} *)

  let pp_name (name : name) (ppf : Format.formatter) : unit =
    match name with
    | Some name -> Format.pp_print_string ppf name
    | None -> Format.pp_print_string ppf "_"

  let pp_name_ann (ty_names : name env) (name : name) (ty : Ty.t) (ppf : Format.formatter) : unit =
    Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (Ty.pp ty_names ty)

  let pp_param (ty_names : name env) (name : name) (ty : Ty.t) (ppf : Format.formatter) : unit =
    Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]" (pp_name name) (Ty.pp ty_names ty)

  let pp : name env -> name env -> t -> Format.formatter -> unit =
    let rec pp ty_names tm_names tm ppf =
      let rec go_params ty_names tm_names (tm : t) ppf =
        match tm with
        | Forall_lit (name, body) ->
            Format.fprintf ppf "@ @[fun@ [%t]@ =>@]%t"
              (pp_name name)
              (go_params (name :: ty_names) tm_names body)
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
              (pp_param ty_names name param_ty)
              (go_params ty_names (name :: tm_names) body)
        | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp ty_names tm_names tm)
      in
      match tm with
      | Let _ as tm ->
          let rec go tm_names (tm : t) ppf =
            match tm with
            | Let (name, def_ty, def, body) ->
                Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                  (pp_name_ann ty_names name def_ty)
                  (pp ty_names tm_names def)
                  (go (name :: tm_names) body)
            | tm -> Format.fprintf ppf "@[%t@]" (pp ty_names tm_names tm)
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
            (pp_app ty_names tm_names head)
            (pp_app ty_names tm_names tm1)
            (pp ty_names tm_names tm2)
      | tm ->
          pp_app ty_names tm_names tm ppf
    and pp_app ty_names tm_names tm ppf =
      let rec go (tm : t) ppf =
        match tm with
        | Forall_app (head, arg) ->
            Format.fprintf ppf "%t@ [%t]"
              (go head)
              (Ty.pp ty_names arg)
        | Fun_app (head, arg) ->
            Format.fprintf ppf "%t@ %t"
              (go head)
              (pp_atomic ty_names tm_names arg)
        | tm ->
            pp_atomic ty_names tm_names tm ppf
      in
      match tm with
      | Forall_app _ | Fun_app _ as tm ->
          Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
      | tm ->
          pp_atomic ty_names tm_names tm ppf
    and pp_atomic ty_names tm_names tm ppf =
      match tm with
      | Var tm_index -> Format.fprintf ppf "%t" (pp_name (List.nth tm_names tm_index))
      | Prim prim -> Format.fprintf ppf "#%s" (Prim.name prim)
      | Int_lit i -> Format.fprintf ppf "%i" i
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Let _ | Forall_lit _ | Forall_app _ | Fun_lit _ | Fun_app _ | Bool_elim _ as tm ->
          Format.fprintf ppf "@[(%t)@]" (pp ty_names tm_names tm)
    in
    pp

end
