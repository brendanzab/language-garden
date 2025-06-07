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


(** Syntax of the core language *)
module Syntax = struct
  (** This module contains a simple, typed representation the syntax of
      the type theory our language is built upon. It’s not intended to be used
      directly, so lacks many convenience features programmers might wish for
      in day-to-day programming.

      As mentioned earlier, a higher level {i surface language} that is easier
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
    | Fun_type of name * ty * ty  (** Dependent function types *)
    | Fun_lit of name * tm        (** Function literals (i.e. lambda expressions) *)
    | Fun_app of tm * tm          (** Function application *)

  (** Currently our type theory only support one {i connective}: function
      types, along with some structural features like variables and let
      bindings and annotated terms. I {i think} you might be able to think of
      the universe to be another connective, but that’s a something to tackle
      another time.

      As more connectives are added to the core language, they should follow
      the same pattern as function types, with separate variants for:

      - type formation: for constructing types that represent the connective
      - introduction: for constructing terms of a given connective
      - elimination: for deconstructing terms of a given connective

      This approach to designing type theories comes from {i natural deduction}.
  *)

  (** Returns [ true ] if the variable is bound anywhere in the term *)
  let rec is_bound (var : index) (tm : tm) : bool =
    match tm with
    | Let (_, def, body) -> is_bound var def || is_bound (var + 1) body
    | Ann (tm, ty) -> is_bound var tm || is_bound var ty
    | Var index -> index = var
    | Univ -> false
    | Fun_type (_, param_ty, body_ty) -> is_bound var param_ty || is_bound (var + 1) body_ty
    | Fun_lit (_, body) -> is_bound (var + 1) body
    | Fun_app (head, arg) -> is_bound var head || is_bound var arg

  let rec fun_lits (tm : tm) : name list * ty =
    match tm with
    | Fun_lit (name, body) ->
        let names, body = fun_lits body
        in name :: names, body
    | body -> [], body

  let fun_apps (tm : tm) : ty * ty list =
    let rec go args tm =
      match tm with
      | Fun_app (head, arg) -> go (arg :: args) head
      | head -> (head, args)
    in
    go [] tm


  (** {1 Pretty printing} *)

  (** This optionally adds back some of the sugar that may have been removed
      during elaboration to make the terms easier to read. Down the line we
      could move this dusugaring step into a {i delaborator}/{i distillation}
      pass that converts core terms back to surface term, and implement a
      pretty printer for the surface language. *)
  let pp ?(resugar = true) : name env -> tm -> Format.formatter -> unit =
    let pp_name name ppf =
      match name with
      | Some name -> Format.pp_print_string ppf name
      | None -> Format.pp_print_string ppf "_"
    in

    let rec pp_tm names tm ppf =
      match tm with
      | Let (_, _, _) as tm ->
          let rec go names tm ppf =
            match tm with
            | Let (name, Ann (def, def_ty), body) when resugar ->
                Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                  (pp_name_ann names name def_ty)
                  (pp_tm names def)
                  (go (name :: names) body)
            | Let (name, def, body) ->
                Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                  (pp_name name)
                  (pp_tm names def)
                  (go (name :: names) body)
            (* Final term should be grouped in a box *)
            | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
          in
          go names tm ppf
      | Ann (tm, ty) ->
          Format.fprintf ppf "@[<2>@[%t :@]@ %t@]"
            (pp_app_tm names tm)
            (pp_tm names ty)
      | Fun_type (None, param_ty, body_ty) when resugar && not (is_bound 0 body_ty) ->
          Format.fprintf ppf "@[%t@ ->@]@ %t"
            (pp_app_tm names param_ty)
            (pp_tm (None :: names) body_ty)
      | Fun_type (_, _, _) as tm ->
          let rec go names tm ppf =
            match tm with
            | Fun_type (None, param_ty, body_ty) when resugar && not (is_bound 0 body_ty) ->
                Format.fprintf ppf "@[%t@ ->@]@ %t"
                  (pp_tm names param_ty)
                  (pp_tm (None :: names) body_ty)
            | Fun_type (name, param_ty, body_ty) ->
                Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]@ %t"
                  (pp_name name)
                  (pp_tm names param_ty)
                  (go (name :: names) body_ty)
            | body_ty ->
                Format.fprintf ppf "@[->@ @[%t@]@]"
                  (pp_tm names body_ty)
          in
          Format.fprintf ppf "@[<4>fun %t@]" (go names tm)
      | Fun_lit (_, _) as tm ->
          let params, body = fun_lits tm in
          Format.fprintf ppf "@[<2>@[<4>fun %a@ =>@]@ @[%t@]@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (Fun.flip pp_name)) params
            (pp_tm (List.rev_append params names) body)
      | tm ->
          pp_app_tm names tm ppf

    and pp_app_tm names tm ppf =
      match tm with
      | Fun_app (_, _) as tm ->
          let head, args = fun_apps tm in
          Format.fprintf ppf "@[<2>%t@ %a@]"
            (pp_atomic_tm names head)
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (Fun.flip (pp_atomic_tm names))) args
      | tm ->
          pp_atomic_tm names tm ppf

    and pp_atomic_tm names tm ppf =
      match tm with
      | Var index -> Format.fprintf ppf "%t" (pp_name (List.nth names index))
      | Univ -> Format.fprintf ppf "Type"
      | tm -> Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)

    and pp_name_ann names name def_ty ppf =
      Format.fprintf ppf "@[<2>@[%t :@]@ %t@]"
        (pp_name name)
        (pp_tm names def_ty)
    in

    pp_tm

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
    | Fun_type of name * vty Lazy.t * (vtm -> vty)
    | Fun_lit of name * (vtm -> vtm)

  (** Neutral terms are terms that could not be reduced to a normal form as a
      result of being stuck on something else that would not reduce further.
      I’m not sure why they are called ‘neutral terms’. Perhaps they are...
      ambivalent about what they might compute to? *)
  and neu =
    | Var of level                (** Variable that could not be reduced further *)
    | Fun_app of neu * vtm Lazy.t  (** Function application *)


  (** {1 Exceptions} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string


  (** {1 Eliminators} *)

  (** The following functions trigger computation if the head term is in the
      appropriate normal form, otherwise queuing up the elimination if the
      term is in a neutral form. *)

  (** Compute a function application *)
  let app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu neu -> Neu (Fun_app (neu, Lazy.from_val arg))
    | Fun_lit (_, body) -> body arg
    | _ -> raise (Error "invalid application")


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : Syntax.tm) : vtm =
    match tm with
    | Syntax.Let (_, def, body) -> eval (eval env def :: env) body
    | Syntax.Ann (tm, _) -> eval env tm
    | Syntax.Var index -> List.nth env index
    | Syntax.Univ ->  Univ
    | Syntax.Fun_type (name, param_ty, body_ty) ->
        let param_ty = lazy (eval env param_ty) in
        let body_ty = fun x -> eval (x :: env) body_ty in
        Fun_type (name, param_ty, body_ty)
    | Syntax.Fun_lit (name, body) -> Fun_lit (name, fun x -> eval (x :: env) body)
    | Syntax.Fun_app (head, arg) -> app (eval env head) (eval env arg)


  (** {1 Quotation} *)

  (** Quotation allows us to convert terms from the semantic domain back into
      syntax. This can be useful to find the normal form of a term, or when
      including terms from the semantics in the syntax during elaboration.

      The size parameter is the number of bindings present in the environment
      where the resulting terms should be bound, allowing us to convert
      variables in the semantic domain back to an {!index} representation
      with {!level_to_size}. It’s important to only use the resulting terms
      at binding depth that they were quoted at. *)
  let rec quote (size : level) (tm : vtm) : Syntax.tm =
    match tm with
    | Neu neu -> quote_neu size neu
    | Univ -> Syntax.Univ
    | Fun_type (name, param_ty, body_ty) ->
        let x = Neu (Var size) in
        Syntax.Fun_type (name, quote size (Lazy.force param_ty), quote (size + 1) (body_ty x))
    | Fun_lit (name, body) ->
        let x = Neu (Var size) in
        Syntax.Fun_lit (name, quote (size + 1) (body x))
  and quote_neu (size : level) (neu : neu) : Syntax.tm =
    match neu with
    | Var level -> Syntax.Var (level_to_index size level)
    | Fun_app (neu, arg) -> Syntax.Fun_app (quote_neu size neu, quote size (Lazy.force arg))


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (size : level) (env : vtm env) (tm : Syntax.tm) : Syntax.tm =
    quote size (eval (env : vtm env) tm)


  (** {1 Conversion Checking} *)

  (** Checks that two values compute to the same term under the assumption that
      both values have the same type. *)
  let rec is_convertible (size : level) (tm1 : vtm) (tm2 : vtm) : bool =
    match tm1, tm2 with
    | Neu neu1, Neu neu2 -> is_convertible_neu size neu1 neu2
    | Univ, Univ -> true
    | Fun_type (_, param_ty1, body_ty1), Fun_type (_, param_ty2, body_ty2) ->
        let x = Neu (Var size) in
        is_convertible size (Lazy.force param_ty1) (Lazy.force param_ty2)
          && is_convertible (size + 1) (body_ty1 x) (body_ty2 x)
    | Fun_lit (_, body1), Fun_lit (_, body2) ->
        let x = Neu (Var size) in
        is_convertible (size + 1) (body1 x) (body2 x)
    (* Eta for functions *)
    | Fun_lit (_, body), fun_tm | fun_tm, Fun_lit (_, body)  ->
        let x = Neu (Var size) in
        is_convertible size (body x) (app fun_tm x)
    | _, _ -> false
  and is_convertible_neu (size : level) (neu1 : neu) (neu2 : neu) =
    match neu1, neu2 with
    | Var level1, Var level2 -> level1 = level2
    | Fun_app (neu1, arg1), Fun_app (neu2, arg2)  ->
        is_convertible_neu size neu1 neu2
          && is_convertible size (Lazy.force arg1) (Lazy.force arg2)
    | _, _ -> false

end
