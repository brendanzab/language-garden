(** {0 Core language} *)

(** {1 Names} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string


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
  | Fun_type of ty * ty
  | Int_type
  | Bool_type

(** Term syntax *)
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * (vtm -> vtm)
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of level          (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of ntm * vtm
    | Bool_elim of ntm * vtm Lazy.t * vtm Lazy.t
    | Prim_app of Prim.t * vtm list


  (** {1 Eliminators} *)

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
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (Int.equal t1 t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.add t1 t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.sub t1 t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.mul t1 t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (Int.neg t1)


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (name, param_ty, body) ->
        Fun_lit (name, param_ty, fun arg -> eval (arg :: env) body)
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm0, tm1) ->
        let head = eval env head in
        let vtm0 = lazy (eval env tm0) in
        let vtm1 = lazy (eval env tm1) in
        bool_elim head vtm0 vtm1
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Bool_elim (head, vtm0, vtm1) ->
        let tm0 = quote size (Lazy.force vtm0) in
        let tm1 = quote size (Lazy.force vtm1) in
        Bool_elim (quote_neu size head, tm0, tm1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

end


(** {1 Pretty printing} *)

let rec pp_ty (ppf : Format.formatter) (ty : ty) : unit =
  match ty with
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf ppf "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty ppf ty
and pp_atomic_ty ppf ty =
  match ty with
  | Int_type -> Format.fprintf ppf "Int"
  | Bool_type -> Format.fprintf ppf "Bool"
  | ty -> Format.fprintf ppf "@[(%a)@]" pp_ty ty

let pp_name_ann ppf (name, ty) =
  Format.fprintf ppf "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param ppf (name, ty) =
  Format.fprintf ppf "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let rec pp_tm (names : name env) (ppf : Format.formatter) (tm : tm) : unit =
  match tm with
  | Let _ as tm ->
      let rec go names ppf tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf ppf "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<v>%a@]" (go names) tm
  | Fun_lit (name, param_ty, body) ->
      let rec go names ppf tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf ppf "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | Bool_elim (head, tm0, tm1) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names ppf tm
and pp_app_tm names ppf tm =
  match tm with
  | Fun_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | Prim_app (prim, args) ->
      let pp_sep ppf () = Format.fprintf ppf "@ " in
      Format.fprintf ppf "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_atomic_tm names ppf tm
and pp_atomic_tm names ppf tm =
  match tm with
  | Var index -> Format.fprintf ppf "%s" (List.nth names index)
  | Int_lit i -> Format.fprintf ppf "%i" i
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | tm -> Format.fprintf ppf "@[(%a)@]" (pp_tm names) tm
