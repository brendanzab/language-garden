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
let level_to_index (size : level) (level : level) =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Syntax} *)

(** Type syntax *)
type ty =
  | Var of index                    (* Local type variables *)
  | Forall_type of name * ty        (* Type of a forall (i.e. the type of a term parameterised by a type) *)
  | Fun_type of ty * ty             (* Type of function types *)
  | Int_type
  | Bool_type

(** Term syntax *)
type tm =
  | Var of index                    (* Local term variables *)
  | Let of name * ty * tm * tm
  | Forall_lit of name * tm         (* Forall literal (i.e. big-lambda abstraction) *)
  | Forall_app of tm * ty           (* Forall application *)
  | Fun_lit of name * ty * tm       (* Function literal (i.e. lambda abstraction) *)
  | Fun_app of tm * tm              (* Function application *)
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  (** Types in weak head normal form (i.e. type values) *)
  type vty =
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)
    | Forall_type of name * (vty -> vty)
    | Fun_type of vty * vty
    | Int_type
    | Bool_type

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
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)
    | Forall_app of ntm * vty
    | Fun_app of ntm * vtm
    | Bool_elim of ntm * (unit -> vtm) * (unit -> vtm)
    | Prim_app of Prim.t * vtm list


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
    | Var ty_index -> List.nth ty_env ty_index
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
    | Var tm_index -> List.nth tm_env tm_index
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
    | Var ty_level -> Var (level_to_index ty_size ty_level)
    | Forall_type (name, body_vty) ->
        let body = quote_vty (ty_size + 1) (body_vty (Var ty_size)) in
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
        let body = quote_vtm (ty_size + 1) tm_size (body (Var ty_size)) in
        Forall_lit (name, body)
    | Fun_lit (name, param_vty, body) ->
        let param_ty = quote_vty ty_size param_vty in
        let body = quote_vtm ty_size (tm_size + 1) (body (Neu (Var tm_size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_ntm (ty_size : level) (tm_size : level) (ntm : ntm) : tm =
    match ntm with
    | Var tm_level ->
        Var (level_to_index tm_size tm_level)
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


  (** {1 Conversion Checking} *)

  let rec is_convertible (ty_size : level) (vty1 : vty) (vty2 : vty) : bool =
    match vty1, vty2 with
    | Var ty_level1, Var ty_level2 -> ty_level1 = ty_level2
    | Forall_type (_, body_ty1), Forall_type (_, body_ty2) ->
        let x : vty = Var ty_size in
        is_convertible (ty_size + 1) (body_ty1 x) (body_ty2 x)
    | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
        is_convertible ty_size param_ty1 param_ty2
          && is_convertible ty_size body_ty1 body_ty2
    | Int_type, Int_type -> true
    | Bool_type, Bool_type -> true
    | _, _ -> false

end


(** {1 Pretty printing} *)

let pp_name (name : name) (ppf : Format.formatter) : unit =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_ty : name env -> ty -> Format.formatter -> unit =
  let rec pp_ty ty_names ty ppf =
    match ty with
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
  and pp_atomic_ty (ty_names : name env) (ty : ty) (ppf : Format.formatter) =
    match ty with
    | Var ty_index -> Format.fprintf ppf "%t" (pp_name (List.nth ty_names ty_index))
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Forall_type _ | Fun_type _ as ty ->
        Format.fprintf ppf "@[(%t)@]" (pp_ty ty_names ty)
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
    match tm with
    | Forall_app (head, arg) ->
        Format.fprintf ppf "@[%t@ [%t]@]"
          (pp_app_tm ty_names tm_names head)
          (pp_ty ty_names arg)
    | Fun_app (head, arg) ->
        Format.fprintf ppf "@[%t@ %t@]"
          (pp_app_tm ty_names tm_names head)
          (pp_atomic_tm ty_names tm_names arg)
    | Prim_app (prim, args) ->
        let pp_sep ppf () = Format.fprintf ppf "@ " in
        Format.fprintf ppf "@[#%s@ %a@]"
          (Prim.name prim)
          (Format.pp_print_list ~pp_sep (Fun.flip (pp_atomic_tm ty_names tm_names))) args
    | tm ->
        pp_atomic_tm ty_names tm_names tm ppf
  and pp_atomic_tm ty_names tm_names tm ppf =
    match tm with
    | Var tm_index -> Format.fprintf ppf "%t" (pp_name (List.nth tm_names tm_index))
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Let _ | Forall_lit _ | Forall_app _ | Fun_lit _ | Fun_app _
    | Bool_elim _ | Prim_app _ as tm ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm ty_names tm_names tm)
  in
  pp_tm
