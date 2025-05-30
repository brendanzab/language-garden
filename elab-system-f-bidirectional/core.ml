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
    | Bool_elim of ntm * vtm Lazy.t * vtm Lazy.t
    | Prim_app of Prim.t * vtm list


  (** {1 Environment entries} *)

  (** Type and term declarations *)
  type decl =
    | Ty_decl
    | Tm_decl of vty

  (** Type and term definitions *)
  type defn =
    | Ty_defn of vty
    | Tm_defn of vtm


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
    | Var index ->
        begin match List.nth env index with
        | Ty_defn vty -> vty
        | Tm_defn _ -> invalid_arg "expected type"
        end
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
    | Var index ->
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
    | Var level -> Var (level_to_index size level)
    | Forall_type (name, body_vty) ->
        let body = quote_vty (size + 1) (body_vty (Var size)) in
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
        let body = quote_vtm (size + 1) (body (Var size)) in
        Forall_lit (name, body)
    | Fun_lit (name, param_vty, body) ->
        let param_ty = quote_vty size param_vty in
        let body = quote_vtm (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_ntm (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
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


  (** {1 Conversion Checking} *)

  let rec is_convertible (size : level) (vty1 : vty) (vty2 : vty) : bool =
    match vty1, vty2 with
    | Var level1, Var level2 -> level1 = level2
    | Forall_type (_, body_ty1), Forall_type (_, body_ty2) ->
        let x : vty = Var size in
        is_convertible (size + 1) (body_ty1 x) (body_ty2 x)
    | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
        is_convertible size param_ty1 param_ty2
          && is_convertible size body_ty1 body_ty2
    | Int_type, Int_type -> true
    | Bool_type, Bool_type -> true
    | _, _ -> false

end


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
  | Var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
  | Int_type -> Format.fprintf fmt "Int"
  | Bool_type -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" (pp_ty names) ty

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
  | Var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
