(** {0 Core language} *)

(** The core language is intended to be minimal, and close to well-understood
    type theories. The majority of this module is split up into the {!Syntax}
    and the {!Semantics}. *)


(** {1 Names} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option


(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** {i De Bruijn index} that represents a variable occurance by the number of
    binders between the occurance and the binder it refers to. *)
    type index = int

(** {i De Bruijn level} that represents a variable occurance by the number of
    binders from the top of the environment to the binder that the ocurrance
    refers to. These do not change their meaning as new bindings are added to
    the environment. *)
type level = int

(** To illustrate the relationship between indices and levels, the level and
    index of the variable [ g ] can be found as follows:

    {[
                    level = 3      index = 1
                │─λ───λ───λ──────▶︎│◀︎─λ──────│
                │                 │         │
      Names     │ λn. λf. λx. n (λg. λh. h (g f)) (λu. x) (λu. u)
      Indices   │ λ   λ   λ   2 (λ   λ   0 (1 3)) (λ   1) (λ   0)
      Levels    │ λ   λ   λ   0 (λ   λ   4 (3 1)) (λ   2) (λ   3)
    ]}
*)

(** Converts a {!level} to an {!index} that is bound in an environment of the
    supplied size. Assumes that [ size > level ]. *)
let level_to_index size level =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by inverting a {!level} using {!level_to_index}. *)
type 'a env = 'a list

(** Nameless variable representations like {i De Bruijn indices} and {i De
    Bruijn levels} have a reputation for off-by-one errors and requiring
    expensive reindexing operations when performing substitutions. Thankfully
    these issues can be largely avoided by choosing different variable
    representations for the {!Syntax} and the {!Semantics}:

    - In the {!Syntax} we represent bound variables with {!index}. This allows
      us to easily compare terms for alpha equivalence and quickly look up
      bindings based on their position in the environment.
    - In the {!Semantics} we represent free variables with {!level}. Because
      the meaning of levels remains the same as new bindings are added to the
      environment, this lets us use terms at greater binding depths without
      needing to reindex them first.

    The only time we really need to reindex terms is when quoting from the
    {!Syntax} back to the {!Semantics}, using the {!level_to_index} function,
    and only requires a single traversal of the term.

    This approach is documented in more detail in Chapter 3 of Andreas Abel’s
    Thesis, {{: https://www.cse.chalmers.se/~abela/habil.pdf} “Normalization
    by Evaluation: Dependent Types and Impredicativity”}. Andras Kovacs also
    explains the approach in {{: https://proofassistants.stackexchange.com/a/910/309}
    an answer on the Proof Assistants Stack Exchange}.
*)


(** Syntax of the core language *)
module Syntax = struct
  (** This module contains a simple, typed representation the syntax of
      the type theory our language is built upon. It’s not intended to be used
      directly, so lacks many convenience features programmers might wish for
      in day-to-day programming.

      As mentioned ealier, a higher level {i surface language} that is easier
      for people to work with directly is defined in another part of this
      implementation.
  *)


  (** Types *)
  type ty = tm

  (** Terms *)
  and tm =
    | Let of name * tm * tm       (** Let bindings (for sharing definitions inside terms) *)
    | Ann of tm * ty              (** Terms annotated with types *)
    | Var of index                (** Variables *)
    | Univ                        (** Universe (i.e. the type of types) *)
    | FunType of name * ty * ty   (** Dependent function types *)
    | FunLit of name * tm         (** Function literals (i.e. lambda expressions) *)
    | FunApp of tm * tm           (** Function application *)

  (** Currently our type theory only support one {i connective}: function
      types, along with some structural features like variables and let
      bindings and annotated terms. I {i think} you might be able to think of
      the universe to be another connective, but that’s a something to tackle
      another time.

      As more connctives are added to the core language, they should follow
      the same pattern as function types, with separate variants for:

      - type formation: for contructing types that represent the connective
      - introduction: for contructing terms of a given connective
      - elimination: for deconstructing terms of a given connective

      This approach to designing type theories comes from {i natural deduction}.
  *)

  (** Returns [ true ] if the variable is bound anywhere in the term *)
  let rec is_bound var = function
    | Let (_, def, body) -> is_bound var def || is_bound (var + 1) body
    | Ann (tm, ty) -> is_bound var tm || is_bound var ty
    | Var index -> index = var
    | Univ -> false
    | FunType (_, param_ty, body_ty) -> is_bound var param_ty || is_bound (var + 1) body_ty
    | FunLit (_, body) -> is_bound (var + 1) body
    | FunApp (head, arg) -> is_bound var head || is_bound var arg

  let rec fun_lits = function
    | FunLit (name, body) ->
        let names, body = fun_lits body
        in name :: names, body
    | body -> [], body

  let fun_apps tm =
    let rec go args = function
      | FunApp (head, arg) -> go (arg :: args) head
      | head -> head, args
    in
    go [] tm


  (** {1 Pretty printing} *)

  (** This optionally adds back some of the sugar that may have been removed
      during elaboration to make the terms easier to read. Down the line we
      could move this dusugaring step into a {i delaborator}/{i distillation}
      pass that converts core terms back to surface term, and implement a
      pretty printer for the surface language. *)
  let pp ?(resugar = true) names =
    let pp_name fmt = function
      | Some name -> Format.pp_print_string fmt name
      | None -> Format.pp_print_string fmt "_"
    in

    let rec pp_tm names fmt = function
      | Let (_, _, _) as tm ->
          let rec go names fmt = function
            | Let (name, Ann (def, def_ty), body) when resugar ->
                Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
                  (pp_name_ann names) (name, def_ty)
                  (pp_tm names) def
                  (go (name :: names)) body
            | Let (name, def, body) ->
                Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
                  pp_name name
                  (pp_tm names) def
                  (go (name :: names)) body
            (* Final term should be grouped in a box *)
            | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
          in
          go names fmt tm
      | Ann (tm, ty) ->
          Format.fprintf fmt "@[<2>@[%a :@]@ %a@]"
            (pp_app_tm names) tm
            (pp_tm names) ty
      | FunType (None, param_ty, body_ty) when resugar && not (is_bound 0 body_ty) ->
          Format.fprintf fmt "@[%a@ ->@]@ %a"
            (pp_app_tm names) param_ty
            (pp_tm (None :: names)) body_ty
      | FunType (_, _, _) as tm ->
          let rec go names fmt = function
            | FunType (None, param_ty, body_ty) when resugar && not (is_bound 0 body_ty) ->
                Format.fprintf fmt "@[%a@ ->@]@ %a"
                  (pp_tm names) param_ty
                  (pp_tm (None :: names)) body_ty
            | FunType (name, param_ty, body_ty) ->
                Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]@ %a"
                  pp_name name
                  (pp_tm names) param_ty
                  (go (name :: names)) body_ty
            | body_ty ->
                Format.fprintf fmt "@[->@ @[%a@]@]"
                  (pp_tm names) body_ty
          in
          Format.fprintf fmt "@[<4>fun %a@]" (go names) tm
      | FunLit (_, _) as tm ->
          let params, body = fun_lits tm in
          Format.fprintf fmt "@[<2>@[<4>fun %a@ :=@]@ @[%a@]@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_name) params
            (pp_tm (List.rev_append params names)) body
      | tm ->
          pp_app_tm names fmt tm

    and pp_app_tm names fmt = function
      | FunApp (_, _) as tm ->
          let head, args = fun_apps tm in
          Format.fprintf fmt "@[<2>%a@ %a@]"
            (pp_atomic_tm names) head
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (pp_atomic_tm names)) args
      | tm ->
          pp_atomic_tm names fmt tm

    and pp_atomic_tm names fmt = function
      | Var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
      | Univ -> Format.fprintf fmt "Type"
      | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm

    and pp_name_ann names fmt (name, def_ty) =
      Format.fprintf fmt "@[<2>@[%a :@]@ %a@]"
        pp_name name
        (pp_tm names) def_ty
    in

    pp_tm names

end

(** Semantics of the core language *)
module Semantics = struct
  (** In this module we implement a ‘semantic model’ of our type theory. This
      gives our language a computational meaning, and allows us to compare
      terms based on this. This will become important for implementing type
      checking and elaboration later on. *)


  (** {1 Semantic domain} *)

  (** We first define some datatypes that represent our terms in a partially
      evaluated form. This is a key part of our approach of managing variable
      bindings during computation, and for aiming to only compute as much as
      is needed during type checking.

      Binding structures are represented using higher-order functions in our
      host language (OCaml) that capture environment at the time where they
      were created. This technique is is often called {i higher-order abstract
      syntax}. This is convenient, but in an actual implementation we might
      want to instead use a pair of the environment and a term from the syntax
      ({i defunctionalising} the closure representation).

      We also make use of OCaml’s {!Lazy.t} type in a number of places to
      explicitly defer some computation until it is absolutely needed.
  *)

  (** Types *)
  type vty = vtm

  (** Terms in weak head normal form *)
  and vtm =
    | Neu of neu                            (** Neutral terms *)
    | Univ
    | FunType of name * vty Lazy.t * (vtm -> vty)
    | FunLit of name * (vtm -> vtm)

  (** Neutral terms are terms that could not be reduced to a normal form as a
      result of being stuck on something else that would not reduce further.
      I’m not sure why they are called ‘neutral terms’. Perhaps they are...
      ambivalent about what they might compute to? *)
  and neu =
    | Var of level                (** Variable that could not be reduced further *)
    | FunApp of neu * vtm Lazy.t  (** Function application *)


  (** {1 Exceptions} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string


  (** {1 Eliminators} *)

  (** The following functions trigger computation if the head term is in the
      appropriate normal form, otherwise queuing up the elimination if the
      term is in a neutral form. *)

  (** Compute a function application *)
  let app head arg =
    match head with
    | Neu neu -> Neu (FunApp (neu, Lazy.from_val arg))
    | FunLit (_, body) -> body arg
    | _ -> raise (Error "invalid application")


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval env = function
    | Syntax.Let (_, def, body) -> eval (eval env def :: env) body
    | Syntax.Ann (tm, _) -> eval env tm
    | Syntax.Var index -> List.nth env index
    | Syntax.Univ ->  Univ
    | Syntax.FunType (name, param_ty, body_ty) ->
        let param_ty = Lazy.from_fun (fun () -> eval env param_ty) in
        let body_ty = fun x -> eval (x :: env) body_ty in
        FunType (name, param_ty, body_ty)
    | Syntax.FunLit (name, body) -> FunLit (name, fun x -> eval (x :: env) body)
    | Syntax.FunApp (head, arg) -> app (eval env head) (eval env arg)


  (** {1 Quotation} *)

  (** Quotation allows us to convert terms from the semantic domain back into
      syntax. This can be useful to find the normal form of a term, or when
      including terms from the semantics in the syntax during elaboration.

      The size parameter is the number of bindings present in the environment
      where the resulting terms should be bound, allowing us to convert
      variables in the semantic domain back to an {!index} representation
      with {!level_to_size}. It’s important to only use the resulting terms
      at binding depth that they were quoted at. *)
  let rec quote size = function
    | Neu neu -> quote_neu size neu
    | Univ -> Syntax.Univ
    | FunType (name, param_ty, body_ty) ->
        let x = Neu (Var size) in
        Syntax.FunType (name, quote size (Lazy.force param_ty), quote (size + 1) (body_ty x))
    | FunLit (name, body) ->
        let x = Neu (Var size) in
        Syntax.FunLit (name, quote (size + 1) (body x))
  and quote_neu size = function
    | Var level -> Syntax.Var (level_to_index size level)
    | FunApp (neu, arg) -> Syntax.FunApp (quote_neu size neu, quote size (Lazy.force arg))


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise size tms tm : Syntax.tm =
    quote size (eval tms tm)


  (** {1 Conversion Checking} *)

  (** Conversion checking is checks if two terms of the same type compute to
      the same term by-definition. *)
  let rec is_convertible size = function
    | Neu neu1, Neu neu2 -> is_convertible_neu size (neu1, neu2)
    | Univ, Univ -> true
    | FunType (_, param_ty1, body_ty1), FunType (_, param_ty2, body_ty2) ->
        let x = Neu (Var size) in
        is_convertible size (Lazy.force param_ty1, Lazy.force param_ty2)
          && is_convertible (size + 1) (body_ty1 x, body_ty2 x)
    | FunLit (_, body1), FunLit (_, body2) ->
        let x = Neu (Var size) in
        is_convertible (size + 1) (body1 x, body2 x)
    (* Eta for functions *)
    | FunLit (_, body), fun_tm | fun_tm, FunLit (_, body)  ->
        let x = Neu (Var size) in
        is_convertible size (body x, app fun_tm x)
    | _, _ -> false
  and is_convertible_neu size = function
    | Var level1, Var level2 -> level1 = level2
    | FunApp (neu1, arg1), FunApp (neu2, arg2)  ->
        is_convertible_neu size (neu1, neu2)
          && is_convertible size (Lazy.force arg1, Lazy.force arg2)
    | _, _ -> false

end
