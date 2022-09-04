(** {0 Elaboration with Record Patching and Singleton Types}

    This is an implementation of a dependently typed language with dependent
    record types, with some additional features intended to make it more
    convenient to use records as first-class modules. It was originally ported
    from {{: https://gist.github.com/mb64/04315edd1a8b1b2c2e5bd38071ff66b5} a
    gist by mb64}.

    The type system is implemented in terms of an ‘elaborator’, which type
    checks and tanslates a user-friendly surface language into a simpler and
    more explicit core language that is more closely connected to type theory.

    {1 Record patching}

    Record patching is a way to constrain the values of fields in a record type.
    Given a record type [ R ], a record patch can be applied using the syntax
    [ R [ l := t; ... ] ]. For example:

    {[
      let Monoid := {
        T : Type;
        empty : T;
        append : T -> T -> T;
      };

      let string-monoid : Monoid [ T := String ] := {
        empty := "";
        append := string-append;
      };
    ]}

    This is like Standard ML’s [ where type ] syntax for {{: https://smlfamily.github.io/sml97-defn.pdf#page=28}
    type realisation}, OCaml’s [ with ] operator for {{: https://v2.ocaml.org/manual/modtypes.html#ss%3Amty-with}
    constraining module types}, and Rust’s [ Iterator<Item = T> ] shorthand
    syntax for {{: https://rust-lang.github.io/rfcs/0195-associated-items.html#constraining-associated-types}
    equality constraints} in type parameter bounds.

    {2 Elaboration of patches to singleton types}

    Patches only exist as a feature of the surface language and are removed
    during elaboration. The expression [ Monoid [ T := String ] ] in the example
    above elaborates to a new record type, where the type of the [ T ] field is
    constrained to [ String ] through the use of a singleton type.

    We also derive the definitions of missing fields in record literals from
    singletons in the expected type. This works nicely in combination with
    record patching. Note in the previous example how we don't need to define
    the field [ T ] in [ string-monoid ].

    With that in mind, the definition of [ string-monoid ] is elaborated to:

    {[
      let string-monoid : {
        T : Type [= String ]; -- singleton type patched here
        empty : T;
        append : T -> T -> T;
      } := {
        T := String; -- definition taken from the singleton
        empty := "";
        append := string-append;
      };
    ]}

    {1 Future work}

    {2 Parser}

    Implement a parser for the surface language.

    {2 Total space conversion}

    CoolTT implements ‘total space conversion’ which automatically converts
    functions in the form [ F : { l : T; ... } -> Type ] to the record type
    [ { l : T; ..., fibre : F { l := l; ... } } ] where necessary. Apparently
    this could help address the ‘bundling problem’, and reduce the need to
    implement implicit function parameters.

    {2 Opaque ascription}

    Adding a ‘sealing operator’ [ e :> T ] to the surface language would allow
    us to opaquely ascribe a type [ T ] to an expression [ e ]. This would
    prevent the contents of the expression from reducing definitionally,
    allowing us to define abstract data types.

    Opaque ascription is sometimes modelled using effects, as seen in th
    language {{:https://people.mpi-sws.org/~rossberg/1ml/} 1ML}. The paper
    {{:https://doi.org/10.1145/3474834} “Logical Relations as Types:
    Proof-Relevant Parametricity for Program Modules”} describes an effectful
    approach based on call-by-push-value that could be useful in the context of
    dependent types.

    Apparently effects are only needed in the presence of mutable references,
    however. If we didn’t need these, we might be able implement sealing by
    hiding definitions behind function parameters. For example:

    - [ ... (e :> T) ... ] elaborates to [ ... (fun (x : T) := x ...) e ]
    - [ ... let x :> T := e; ... ] elaborates to [ ... ((fun (x : T) := ...) e) ]

    {2 Metavariables and unification}

    Implicit function types and unification could be convenient. This could be
    challenging to implement in the presence coercive subtyping, however.
    Apparently total space conversion addresses some of the same pain points as
    implicit parameters, but I'm still somewhat skeptical of this!

    {2 Patches elaborate to large, unfolded terms}

    Each patch currently elaborates to a copy of the original record type. This
    a problem for error messages, where the type ends up fully unfolded and to
    understand, and it could become a performance issue down the line when
    elaborating and compiling larger programs.

    A distiller could attempt to convert singletons back to patches for better
    error messages, but to really address the usability and performance issues
    we might ultimately need to add patches to the core language and control the
    level of unfolding with glued evaluation.

    {2 Use patch syntax for record literal updates}

    The same syntax used by patches could be used as a way to update the fields
    of record literals.

    {1 Related work}

    This implementation is heavily based on {{: https://gist.github.com/mb64/04315edd1a8b1b2c2e5bd38071ff66b5}
    mb64’s sketch implementation in Haskell}, but contains various bug fixes,
    alterations, and extensions.

    Record patching was originally proposed and implemented for CoolTT in the
    setting of cubical type theory:

    - {{: https://github.com/RedPRL/cooltt/issues/266} Record patching (like
      SML [ where type ])}
    - {{: https://github.com/RedPRL/cooltt/issues/267} Support for auto-
      converting between fibered and parameterized type families}

    Reed Mullanix's presentation from WITS’22, {{: https://www.youtube.com/watch?v=1_ZJIYu2BRk}
    Setting the Record Straight with Singletons} (slides {{: https://cofree.coffee/~totbwf/slides/WITS-2022.pdf}
    here}) provides a good description of the approach taken in CoolTT, which
    continues to be developed and improved.

    Elaborating record patches to singleton types is similar to approaches
    developed for formalising and implementing type realisation in Standard ML,
    for example in {{: https://doi.org/10.1145/1183278.1183281} “Extensional
    equivalence and singleton types”}. Unlike this work, we avoid defining
    singletons in terms of extensional equality, which makes it much easier to
    maintain decideable type checking.
*)


(** Extensions to the {!Stdlib.List} module *)
module List : sig

  include module type of List

  (** Returns the index of the given element in the list *)
  val elem_index : 'a -> 'a list -> int option

  (** Returns a list of duplicate elements in a list *)
  val find_dupes : 'a list -> 'a list

end = struct

  include List

  let elem_index a xs =
    let rec go i = function
      | [] -> None
      | x :: xs -> if x = a then Some i else go (i + 1) xs in
    go 0 xs

  let find_dupes xs =
    let rec go dupes = function
      | [] -> List.rev dupes
      | x :: xs when List.mem x xs && not (List.mem x dupes) -> go (x :: dupes) xs
      | _ :: xs -> go dupes xs in
    go [] xs

end


(** Core language *)
module Core = struct
  (** The core language is intended to be minimal, and close to well-understood
      type theories. The majority of this module is split up into the {!Syntax}
      and the {!Semantics}. *)


  (** {1 Names} *)

  (** Labels are significant to the equality of terms. They are typically used
      in the fields of records, and in record projections. *)
  type label = string

  (** These names are used as hints for pretty printing binders and variables,
      but don’t impact the equality of terms. *)
  type name = string


  (** {1 Nameless binding structure} *)

  (** The binding structure of terms is represented in the core language by
      using numbers that represent the distance to a binder, instead of by the
      names attached to those binders. *)

  (** {i De Bruijn index} that represents a variable by the number of binders
      between the variable and the binder it refers to. *)
  type index = int

  (** {i De Bruijn level} that represents a variable by the number of binders
      from the top of the environment to the binder that it refers to. These do
      not change their meaning as new bindings are added to the environment. *)
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

    (** For documenting where variables are bound *)
    type 'a binds = 'a

    (** Types *)
    type ty = tm

    (** Terms *)
    and tm =
      | Let of name * tm * tm
      | Var of index
      | Ann of tm * ty
      | Univ
      | FunType of name * ty * (ty binds)
      | FunLit of name * (tm binds)
      | FunApp of tm * tm
      | RecType of decls
      | RecLit of (label * tm) list
      | RecProj of tm * label
      | SingType of ty * tm               (** Singleton type former: [ A [= x ] ] *)
      | SingIntro of tm                   (** Singleton introduction: [ #sing-intro x ] *)
      | SingElim of tm * tm               (** Singleton elimination: [ #sing-elim x a ] *)

    (** Each ‘connective’ in the core language follows a similar pattern, with
        separate variants for:

        - type formation: for contructing types that represent the connective
        - introduction: for contructing terms of a given connective
        - elimination: for deconstructing terms of a given connective

        In the case of [ #sing-elim x a ], the [ a ] is used for evaluation
        during type checking, but is intended to be erased in compiled code.
    *)

    (** Field declarations *)
    and decls =
      | Nil
      | Cons of label * ty * (decls binds)


    (** Returns [ true ] if the variable is bound anywhere in the term. *)
    let rec is_bound var = function
      | Let (_, def, body) -> is_bound var def || is_bound (var + 1) body
      | Ann (tm, ty) -> is_bound var tm || is_bound var ty
      | Var index -> index = var
      | Univ -> false
      | FunType (_, param_ty, body_ty) -> is_bound var param_ty || is_bound (var + 1) body_ty
      | FunLit (_, body) -> is_bound (var + 1) body
      | FunApp (head, arg) -> is_bound var head || is_bound var arg
      | RecType decls -> is_bound_decls var decls
      | RecLit defns -> List.exists (fun (_, tm) -> is_bound var tm) defns
      | RecProj (head, _) -> is_bound var head
      | SingType (ty, sing_tm) -> is_bound var ty || is_bound var sing_tm
      | SingIntro tm -> is_bound var tm
      | SingElim (tm, sing_tm) -> is_bound var tm || is_bound var sing_tm
    and is_bound_decls var = function
      | Nil -> false
      | Cons (_, ty, decls) ->
          is_bound var ty || is_bound_decls (var + 1) decls


    (** {1 Pretty printing} *)

    let pretty names tm =
      let concat = String.concat "" in
      let parens wrap s = if wrap then concat ["("; s; ")"] else s in
      let rec go wrap names = function
        | Let (name, Ann (def, def_ty), body) ->
            parens wrap (concat ["let "; name; " : "; go false names def_ty; " := ";
              go false names def; "; "; go false (name :: names) body])
        | Let (name, def, body) ->
            parens wrap (concat ["let "; name; " := "; go false names def; "; ";
              go false (name :: names) body])
        | Var index -> List.nth names index
        | Ann (tm, ty) -> parens wrap (concat [go true names tm; " : "; go false names ty])
        | Univ -> "Type"
        | FunType (name, param_ty, body_ty) ->
            if is_bound 0 body_ty then
              parens wrap (concat ["fun ("; name; " : "; go false names param_ty;
                ") -> "; go false (name :: names) body_ty])
            else
              parens wrap (concat [go false names param_ty; " -> "; go false ("" :: names) body_ty])
        | FunLit (name, body) ->
            parens wrap (concat ["fun "; name; " := ";
              go false (name :: names) body])
        | FunApp (head, arg) ->
            parens wrap (concat [go false names head; " ";  go true names arg])
        | RecType Nil | RecLit [] -> "{}"
        | RecType decls -> concat ["{ "; go_decls names decls; "}" ]
        | RecLit defns ->
            concat ["{ ";
              concat (List.map
                (fun (label, tm) -> concat [label; " := "; go false names tm; "; "])
                defns);
              "}"]
        | RecProj (head, label) -> concat [go false names head; "."; label]
        | SingType (ty, sing_tm) ->
            parens wrap (concat [go false names ty; " [= ";
              go false names sing_tm; " ]"])
        | SingIntro tm -> parens wrap (concat ["#sing-intro "; go true names tm])
        | SingElim (tm, sing_tm) ->
            parens wrap (concat ["#sing-elim "; go true names tm; " ";
              go true names sing_tm])
      and go_decls names = function
        | Nil -> ""
        | Cons (label, ty, decls) ->
            concat [label; " : "; go false names ty; "; ";
              go_decls (label :: names) decls]
      in
      go false names tm

  end

  (** Semantics of the core language *)
  module Semantics = struct

    (** {1 Semantic domain} *)

    (** The following data structures represent the semantic interpretation of
        the core syntax. *)

    (** Types *)
    type ty = tm

    (** Terms in weak head normal form *)
    and tm =
      | Neu of neu                          (** Neutral terms *)
      | Univ
      | FunType of name * ty * (tm -> ty)
      | FunLit of name * (tm -> tm)
      | RecType of decls
      | RecLit of (label * tm) list
      | SingType of ty * tm
      | SingIntro                           (** Singleton introduction, with term erased *)

    (** Field declarations *)
    and decls =
      | Nil
      | Cons of label * ty * (tm -> decls)

    (** Neutral terms are terms that could not be reduced to a normal form as a
        result of being stuck on something else that would not reduce further.
        I’m not sure why they are called ‘neutral terms’. Perhaps they are...
        ambivalent about what they might compute to? *)
    and neu =
      | Var of level                        (** Variable that could not be reduced further *)
      | FunApp of neu * tm                  (** Function application *)
      | RecProj of neu * label              (** Record projection *)

    (** An environment of bindings that can be looked up directly using a
        {!Syntax.index}, or by inverting a {!level} using {!level_to_index}. *)
    type 'a env = 'a list


    (** {1 Error handling} *)

    (** An error that was encountered during computation. This should only ever
        be raised if ill-typed terms were supplied to the semantics. *)
    exception Error of string

    (** Raises an {!Error} exception *)
    let error message =
      raise (Error message)


    (** {1 Eliminators} *)

    (** The following functions trigger computation if the head term is in the
        appropriate normal form, otherwise queuing up the elimination if the
        term is in a neutral form. *)

    (** Compute a function application *)
    let app head arg =
      match head with
      | Neu neu -> Neu (FunApp (neu, arg))
      | FunLit (_, body) -> body arg
      | _ -> error "invalid application"

    (** Compute a record projection *)
    let proj head label =
      match head with
      | RecLit defns -> defns |> List.find (fun (l, _) -> l = label) |> snd
      | Neu neu -> Neu (RecProj (neu, label))
      | _ -> error "invalid projection"


    (** {1 Finding the types of record projections} *)

    (** Returns the type of a record projection *)
    let proj_ty head decls label =
      let rec go decls =
        match decls with
          | Nil -> None
          | Cons (l, ty, _) when l = label -> Some ty
          | Cons (l, _, decls) -> go (decls (proj head l))
      in
      go decls


    (** {1 Evaluation} *)

    (** Evaluate a term from the syntax into its semantic interpretation *)
    let rec eval tms : Syntax.tm -> tm = function
      | Syntax.Let (_, def, body) -> eval (eval tms def :: tms) body
      | Syntax.Var index -> List.nth tms index
      | Syntax.Ann (tm, _) -> eval tms tm
      | Syntax.Univ -> Univ
      | Syntax.FunType (name, param_ty, body_ty) ->
          FunType (name, eval tms param_ty, fun x -> eval (x :: tms) body_ty)
      | Syntax.FunLit (name, body) -> FunLit (name, fun x -> eval (x :: tms) body)
      | Syntax.FunApp (head, arg) -> app (eval tms head) (eval tms arg)
      | Syntax.RecType decls -> RecType (eval_decls tms decls)
      | Syntax.RecLit defns ->
          RecLit (List.map (fun (label, expr) -> (label, eval tms expr)) defns)
      | Syntax.RecProj (head, label) -> proj (eval tms head) label
      | Syntax.SingType (ty, sing_tm) -> SingType (eval tms ty, eval tms sing_tm)
      | Syntax.SingIntro _ -> SingIntro
      | Syntax.SingElim (_, sing_tm) -> eval tms sing_tm
    and eval_decls tms : Syntax.decls -> decls = function
      | Syntax.Nil -> Nil
      | Syntax.Cons (label, ty, decls) ->
          Cons (label, eval tms ty, fun x -> eval_decls (x :: tms) decls)


    (** {1 Quotation} *)

    (** Quotation allows us to convert terms from semantic domain back into
        syntax. This can be useful to find the normal form of a term, or when
        including terms from the semantics in the syntax during elaboration.

        The size parameter is the number of bindings present in the environment
        where we the resulting terms should be bound, allowing us to convert
        variables in the semantic domain back to an {!index} representation
        with {!level_to_size}. It’s important to only use the resulting terms
        at binding depth that they were quoted at.

        Quotation is type directed, but we currently only use types as a way to
        restore the values of singletons that were erased during evaluation. The
        typing environment is used to recover the types of variables. We could
        alternatively add type annotations to neutral terms to avoid needing to
        supply this argument.
    *)
    let rec quote size tys tm ty : Syntax.tm =
      match tm with
      | Neu neu -> fst (quote_neu size tys neu)
      | Univ -> Syntax.Univ
      | FunType (name, param_ty, body_ty) ->
          let var = Neu (Var size) in
          let param_ty = quote size tys param_ty Univ in
          let body_ty = quote (size + 1) (Univ :: tys) (body_ty var) Univ in
          Syntax.FunType (name, param_ty, body_ty)
      | FunLit (name, body) ->
          begin match ty with
          | FunType (_, param_ty, body_ty) ->
              let var = Neu (Var size) in
              Syntax.FunLit (name, quote (size + 1) (param_ty :: tys) (body var) (body_ty var))
          | _ -> error "not a function type"
          end
      | RecType decls -> Syntax.RecType (quote_decls size tys decls)
      | RecLit defns ->
          begin match ty with
          | RecType decls ->
              let rec go defns decls =
                match defns, decls with
                | [], Nil -> []
                | (label, tm) :: defns, Cons (label', ty, decls) when label = label' ->
                    (label, quote size tys tm ty) :: go defns (decls tm)
                | _, _ -> error "mismatched fields"
              in
              Syntax.RecLit (go defns decls)
          | _ -> error "not a record type"
          end
      | SingType (ty, sing_tm) ->
          Syntax.SingType (quote size tys ty Univ, quote size tys sing_tm ty)
      | SingIntro ->
          begin match ty with
          (* Restore the erased term from the singleton type *)
          | SingType (ty, sing_tm) -> Syntax.SingIntro (quote size tys ty sing_tm)
          | _ -> error "not a singleton type"
          end
    and quote_neu size tys : neu -> Syntax.tm * ty = function
      | Var level ->
          let index = level_to_index size level in
          (Syntax.Var index, List.nth tys index)
      | FunApp (head, arg) ->
          begin match quote_neu size tys head with
          | head, FunType (_, param_ty, body_ty) ->
              (Syntax.FunApp (head, (quote size tys arg param_ty)), body_ty arg)
          | _ -> error "not a function type"
          end
      | RecProj (head, label) ->
          begin match quote_neu size tys head with
          | head', RecType decls ->
              let ty = proj_ty (Neu head) decls label |> Option.get in
              (Syntax.RecProj (head', label), ty)
          | _ -> error "not a record type"
          end
    and quote_decls size tys : decls -> Syntax.decls = function
      | Nil -> Syntax.Nil
      | Cons (label, ty, decls) ->
          let var = Neu (Var size) in
          Syntax.Cons (label, quote size tys ty Univ,
            quote_decls (size + 1) (ty :: tys) (decls var))


    (** {1 Normalisation} *)

    (** By evaluating a term then quoting the result, we can produce a term that
        is reduced as much as possible in the current environment. *)
    let normalise size tms tys tm ty : Syntax.tm =
      quote size tys (eval tms tm) ty


    (** {1 Conversion Checking} *)

    (** Conversion checking tests to see if two terms are are equal by checking
        if they compute to the same term. This could be implemented by reading
        back both values and checking for alpha-equivalence, but it’s faster to
        do this all at once.

        Type-directed conversion allows us to support full eta for unit types,
        which show up in our language as empty records and singletons. If we
        wanted to stick to untyped conversion checking, according to Andras
        Korvacs we could alternatively:

        - perform best-effort eta, where unit elements are the same as anything
        - detect definitionally irrelevant types during elaboration, marking
          irrelevant terms

        As with {!quote}, the typing environment is used to recover the types of
        variables.
    *)
    let rec is_convertible size tys tm1 tm2 : ty -> bool = function
      | Neu _ ->
          begin match tm1, tm2 with
          | Neu n1, Neu n2 -> Option.is_some (is_convertible_neu size tys n1 n2)
          (* Neutral types are abstract, so their inhabitants should not have reduced *)
          | _, _ -> error "not a neutral"
          end
      | Univ ->
          begin match tm1, tm2 with
          | Univ, Univ -> true
          | Neu neu1, Neu neu2 -> Option.is_some (is_convertible_neu size tys neu1 neu2)
          | FunType (_, param_ty1, body_ty1), FunType (_, param_ty2, body_ty2) ->
              let var = Neu (Var size) in
              is_convertible size tys param_ty1 param_ty2 Univ
                && is_convertible (size + 1) (param_ty1 :: tys) (body_ty1 var) (body_ty2 var) Univ
          | RecType (decls1), RecType (decls2) ->
              is_convertible_decls size tys decls1 decls2
          | SingType (ty1, sing_tm1), SingType (ty2, sing_tm2) ->
              is_convertible size tys ty1 ty2 Univ
                && is_convertible size tys sing_tm1 sing_tm2 ty1
          | _, _ -> false
          end
      | FunType (_, param_ty, body_ty) ->
          (* Eta for functions *)
          let var = Neu (Var size) in
          is_convertible (size + 1) (param_ty :: tys) (app tm1 var) (app tm2 var) (body_ty var)
      | RecType decls ->
          (* Eta for records

            Record patching introduces subtyping problems that go inside records.
            With coercive subtyping (implemented in {!Surface.coerce}), record
            eta expansions are sometimes introduced in the core language that
            weren't present in the source syntax. This means that for usability
            it helps to include eta for records.
          *)
          let rec go decls =
            match decls with
            | Cons (label, ty, decls) ->
                let tm1 = proj tm1 label in
                let tm2 = proj tm2 label in
                is_convertible size tys tm1 tm2 ty && go (decls tm1)
            | Nil -> true (* Pretty sure eta for units is hidden in here! *)
          in
          go decls
      | SingType (_, _) -> true
      | _ -> error "not a type"
    and is_convertible_neu size tys neu1 neu2 =
      match neu1, neu2 with
      | Var level1, Var level2 when level1 = level2 -> Some (List.nth tys (level_to_index size level1))
      | FunApp (func1, arg1), FunApp (func2, arg2) ->
          begin match is_convertible_neu size tys func1 func2 with
          | Some (FunType (_, param_ty, body_ty)) ->
              if is_convertible size tys arg1 arg2 param_ty then Some (body_ty arg1) else None
          | Some _ -> error "not a function type"
          | None -> None
          end
      | RecProj (record1, label1), RecProj (record2, label2) when label1 = label2 ->
          begin match is_convertible_neu size tys record1 record2 with
          | Some (RecType decls) -> proj_ty (Neu record1) decls label1
          | Some _ -> error "not a record type"
          | None -> None
          end
      | _, _ -> None
    and is_convertible_decls size tys decls1 decls2 =
      match decls1, decls2 with
      | Nil, Nil -> true
      | Cons (label1, ty1, decls1), Cons (label2, ty2, decls2) when label1 = label2 ->
          let var = Neu (Var size) in
          is_convertible size tys ty1 ty2 Univ
            && is_convertible_decls (size + 1) (ty1 :: tys) (decls1 var) (decls2 var)
      | _, _ -> false

  end
end

(** Surface language *)
module Surface = struct
  (** The surface language closely mirrors what the programmer originaly wrote,
      including syntactic sugar, subtyping, and other higher level language
      features that make programming more convenient (in comparison to the
      {!Core.Syntax}). *)


  (** {1 Surface Syntax} *)

  (** Terms in the surface language *)
  type tm =
    | Let of string * tm option * tm * tm    (** Let expressions: [ let x : A := t; f x ] *)
    | Name of string                         (** References to named things: [ x ] *)
    | Ann of tm * tm                         (** Terms annotated with types: [ x : A ] *)
    | Univ                                   (** Universe of types: [ Type ] *)
    | FunType of (string * tm) list * tm     (** Function types: [ fun (x : A) -> B x ] *)
    | FunArrow of tm * tm                    (** Function arrow types: [ A -> B ] *)
    | FunLit of string list * tm             (** Function literals: [ fun x := f x ] *)
    | RecType of (string * tm) list          (** Record types: [ { x : A; ... } ]*)
    | RecLit of (string * tm) list           (** Record literals: [ { x := A; ... } ]*)
    | RecUnit                                (** Unit records: [ {} ] *)
    | SingType of tm * tm                    (** Singleton types: [ A [= x ] ] *)
    | App of tm * tm list                    (** Applications: [ f x ] *)
    | Proj of tm * string list               (** Projections: [ r.l ] *)
    | Patch of tm * (string * tm) list       (** Record patches: [ R [ B := A; ... ] ] *)

  (** We don’t need to add syntax for introducing and eliminating singletons in
      the surface language. These are instead added implicitly during
      elaboration to the core language. For example:

      - If we have [ x : Nat ] and [ x ≡ 7 : Nat ], we can elaborate the term
        [ x : A [= 7 ] ] into [ #sing-intro x : A [= 7 ] ]
      - If we have [ x : Nat [= 7 ] ], we can elaborate the term [ x : Nat ]
        into [ #sing-elim x 7 : Nat ]
  *)


  (** {1 Elaboration } *)

  (** This is where we implement user-facing type checking, in addition to
      translating the convenient surface language into a simpler, more explicit
      core language.

      While we {e could} translate syntactic sugar in the parser, by leaving
      this to elaboration time we make it easier to report higher quality error
      messages that are more relevant to what the programmer originally wrote.
  *)

  module Syntax = Core.Syntax
  module Semantics = Core.Semantics


  (** {2 Elaboration state} *)

  (** The elaboration context records the bindings that are currently bound at
      the current scope in the program. The environments are unzipped to make it
      more efficient to call functions from {!Core.Semantics}. *)
  type context = {
    size : Core.level;                  (** Number of entries bound. *)
    names : Core.name Semantics.env;    (** Name environment *)
    tys : Semantics.ty Semantics.env;   (** Type environment *)
    tms : Semantics.tm Semantics.env;   (** Term environment *)
  }

  (** The initial elaboration context, without any bindings *)
  let initial_context = {
    size = 0;
    names = [];
    tys = [];
    tms = [];
  }

  (** Returns the next variable that will be bound in the context after calling
      {!bind_def} or {!bind_param} *)
  let next_var context =
    Semantics.Neu (Semantics.Var context.size)

  (** Binds a definition in the context *)
  let bind_def context name ty tm = {
    size = context.size + 1;
    names = name :: context.names;
    tys = ty :: context.tys;
    tms = tm :: context.tms;
  }

  (** Binds a parameter in the context *)
  let bind_param context name ty =
    bind_def context name ty (next_var context)

  (** {3 Functions related to the core semantics} *)

  (** These wrapper functions make it easier to call functions from the
      {!Core.Semantics} using state from the elaboration context. *)

  let eval context : Syntax.tm -> Semantics.tm =
    Semantics.eval context.tms
  let quote context : Semantics.tm -> Semantics.ty -> Syntax.tm =
    Semantics.quote context.size context.tys
  let is_convertible context : Semantics.tm -> Semantics.tm -> Semantics.ty -> bool =
    Semantics.is_convertible context.size context.tys
  let pretty context : Syntax.tm -> string =
    Syntax.pretty context.names
  let pretty_quoted context tm ty : string =
    pretty context (quote context tm ty)


  (** {2 Elaboration errors} *)

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors or a missing type annotations. This is
      normal, and should be rendered nicely to the programmer. *)
  exception Error of string

  (** Raises an {!Error} exception *)
  let error message =
    raise (Error message)


  (** {2 Coercive subtyping} *)

  (** Coerces a term from one type to another type. By performing coercions
      during elaboration we avoid having to introduce subtyping in the core
      language. *)
  let rec coerce context tm from_ty to_ty : Syntax.tm =
    match from_ty, to_ty with
    (* No need to coerce the term if both types are already the same! *)
    | from_ty, to_ty when is_convertible context from_ty to_ty Semantics.Univ -> tm
    (* Coerce the term to a singleton with {!Syntax.SingIntro}, if the term is
      convertible to the term expected by the singleton *)
    | from_ty, Semantics.SingType (to_ty, sing_tm) ->
        let tm = coerce context tm from_ty to_ty in
        let tm' = eval context tm in
        if is_convertible context sing_tm tm' to_ty then Syntax.SingIntro tm else
          let expected = pretty_quoted context sing_tm to_ty in
          let found = pretty_quoted context tm' from_ty in
          let ty = pretty_quoted context to_ty Semantics.Univ in
          error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`")
    (* Coerce the singleton back to its underlying term with {!Syntax.SingElim}
      and attempt further coercions from its underlying type *)
    | Semantics.SingType (from_ty, sing_tm), to_ty ->
        let tm = Syntax.SingElim (tm, quote context sing_tm from_ty) in
        coerce context tm from_ty to_ty
    (* Coerce the fields of a record with record eta expansion *)
    | Semantics.RecType from_decls, Semantics.RecType to_decls ->
        let rec go from_decls to_decls =
          match from_decls, to_decls with
          | Semantics.Nil, Semantics.Nil -> []
          (* Use eta-expansion to coerce fields that share the same label *)
          | Semantics.Cons (from_label, from_ty, from_decls)
          , Semantics.Cons (to_label, to_ty, to_decls) when from_label = to_label ->
              let from_tm = eval context (Syntax.RecProj (tm, from_label)) in
              let to_tm = coerce context (Syntax.RecProj (tm, from_label)) from_ty to_ty in
              (to_label, to_tm) :: go (from_decls from_tm) (to_decls (eval context to_tm))
          (* When the type of the target field is a singleton we can use it to
             fill in the definition of a missing field in the source term. This
             is similar to how we handle missing fields in {!check}. *)
          | from_decls, Semantics.Cons (to_label, Semantics.SingType (to_ty, sing_tm), to_decls) ->
              let to_tm = Syntax.SingIntro (quote context sing_tm to_ty) in
              (to_label, to_tm) :: go from_decls (to_decls (eval context to_tm))
          | Semantics.Cons (from_label, _, _), Semantics.Cons (to_label, _, _) ->
              error ("type mismatch: expected field `" ^ to_label ^ "`, found field `" ^ from_label ^ "`")
          | _, _ -> Semantics.error "mismatched telescope length"
        in
        Syntax.RecLit (go from_decls to_decls)
    (* TODO: subtyping for functions! *)
    | from_ty, to_ty  ->
        let expected = pretty_quoted context to_ty Semantics.Univ in
        let found = pretty_quoted context from_ty Semantics.Univ in
        error ("type mismatch: expected `" ^ expected ^ "`, found `" ^ found ^ "`")


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors, and provide enough {i control} to the
      algorithm to keep type inference deciable even in the presence of ‘fancy’
      types, for example dependent types, higher rank types, and subtyping. *)

  (** Elaborate a term in the surface language into a term in the core language
      in the presence of a type annotation. *)
  let rec check context tm ty : Syntax.tm =
    match tm, ty with
    (* Let expressions *)
    | Let (name, def_ty, def, body), ty ->
        let def, def_ty =
          match def_ty with
          | None -> infer context def
          | Some def_ty ->
              let def_ty = check context def_ty Semantics.Univ in
              let def_ty' = eval context def_ty in
              let def = check context def def_ty' in
              (Syntax.Ann (def, def_ty), def_ty')
        in
        let context = bind_def context name def_ty (eval context def) in
        Syntax.Let (name, def, check context body ty)

    (* Function literals *)
    | FunLit (names, body), ty ->
        (* Iterate over the parameters of the function literal, constructing a
           function literal in the core language. *)
        let rec go context names ty =
          match names, ty with
          | [], body_ty -> check context body body_ty
          | name :: names, Semantics.FunType (_, param_ty, body_ty) ->
              let var = next_var context in
              let context = bind_def context name param_ty var in
              Syntax.FunLit (name, go context names (body_ty var))
          | _, _ -> error "too many parameters in function literal"
        in
        go context names ty

    (* Record literals *)
    | RecLit defns, Semantics.RecType decls ->
        (* TODO: elaborate fields out of order? *)
        let rec go defns decls =
          match defns, decls with
          | [], Semantics.Nil -> []
          | (label, tm) :: defns, Semantics.Cons (label', ty, decls) when label = label' ->
              let tm = check context tm ty in
              (label, tm) :: go defns (decls (eval context tm))
          (* When the expected type of a field is a singleton we can use it to
             fill in the definition of a missing fields in the record literal. *)
          | defns, Semantics.Cons (label, Semantics.SingType (ty, sing_tm), decls) ->
              let tm = Syntax.SingIntro (quote context sing_tm ty) in
              (label, tm) :: go defns (decls (eval context tm))
          | _, Semantics.Cons (label, _, _) -> error ("field `" ^ label ^ "` is missing from record literal")
          | (label, _) :: _, Semantics.Nil -> error ("unexpected field `" ^ label ^ "` in record literal")
        in
        Syntax.RecLit (go defns decls)

    (* Records with no entries. These are ambiguous and need to be disambuguated
       with a type annotation. *)
    | RecUnit, Semantics.Univ ->
        Syntax.RecType Syntax.Nil
    | RecUnit, Semantics.RecType Semantics.Nil ->
        Syntax.RecLit []

    (* Singleton introduction. No need for any syntax in the surface language
       here, instead we use the type annotation to drive this. *)
    | tm, Semantics.SingType (ty, sing_tm) ->
        let tm = check context tm ty in
        let tm' = eval context tm in
        if is_convertible context sing_tm tm' ty then Syntax.SingIntro tm else
          let expected = pretty_quoted context sing_tm ty in
          let found = pretty_quoted context tm' ty in
          let ty = pretty_quoted context ty Semantics.Univ in
          error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`")

    (* For anything else, try inferring the type of the term, then attempting to
       coerce the term to the expected type. *)
    | tm, ty ->
        let tm, ty' = infer context tm in
        let tm, ty' = elim_implicits context tm ty' in
        coerce context tm ty' ty

  (** Elaborate a term in the surface language into a term in the core language,
      inferring its type. *)
  and infer context : tm -> Syntax.tm * Semantics.ty = function
    (* Let expressions *)
    | Let (name, def_ty, def, body) ->
        let def, def_ty =
          match def_ty with
          | None -> infer context def
          | Some def_ty ->
              let def_ty = check context def_ty Semantics.Univ in
              let def_ty' = eval context def_ty in
              let def = check context def def_ty' in
              (Syntax.Ann (def, def_ty), def_ty')
        in
        let context = bind_def context name def_ty (eval context def) in
        let body, body_ty = infer context body in
        (Syntax.Let (name, def, body), body_ty)

    (* Named terms *)
    | Name name ->
        (* Find the index of most recent binding in the context identified by
           [name], starting from the most recent binding. This gives us the
           corresponding de Bruijn index of the variable. *)
        begin match List.elem_index name context.names with
        | Some index -> (Syntax.Var index, List.nth context.tys index)
        | None -> error ("`" ^ name ^ "` is not bound in the current scope")
        end

    (* Annotated terms *)
    | Ann (tm, ty) ->
        let ty = check context ty Semantics.Univ in
        let ty' = eval context ty in
        let tm = check context tm ty' in
        (Syntax.Ann (tm, ty), ty')

      (* Universes *)
    | Univ ->
        (* We use [Type : Type] here for simplicity, which means this type
           theory is inconsistent. This is okay for a toy type system, but we’d
           want look into using universe levels in an actual implementation. *)
        (Syntax.Univ, Semantics.Univ)

    (* Function types *)
    | FunType (params, body_ty) ->
        let rec go context = function
          | [] -> check context body_ty Semantics.Univ
          | (name, param_ty) :: params ->
              let param_ty = check context param_ty Semantics.Univ in
              let context = bind_param context name (eval context param_ty) in
              Syntax.FunType (name, param_ty, go context params)
        in
        (go context params, Semantics.Univ)

    (* Arrow types. These are implemented as syntactic sugar for non-dependent
       function types. *)
    | FunArrow (param_ty, body_ty) ->
        (* Arrow types are implemented as syntactic sugar for non-dependent
           function types. *)
        let param_ty = check context param_ty Semantics.Univ in
        let context = bind_param context "_" (eval context param_ty) in
        let body_ty = check context body_ty Semantics.Univ in
        (Syntax.FunType ("_", param_ty, body_ty), Semantics.Univ)

    (* Function literals. These do not have type annotations on their arguments
       and so we don’t know ahead of time what types to use for the arguments
       when adding them to the context. As a result with coose to throw an
       ambiguity error here. *)
    | FunLit (_, _) -> error "ambiguous function literal"

    (* Function application *)
    | RecType decls ->
        let rec go context seen_labels = function
          | [] -> (Syntax.Nil)
          | (label, _) :: _ when List.mem label seen_labels ->
              error ("duplicate label `" ^ label ^ "` in record type")
          | (label, ty) :: decls ->
              let ty = check context ty Semantics.Univ in
              let context = bind_param context label (eval context ty) in
              Syntax.Cons (label, ty, go context (label :: seen_labels) decls)
        in
        (Syntax.RecType (go context [] decls), Semantics.Univ)

    (* Unit records. These are ambiguous in inference mode. We could default to
       one or the other, and perhaps coerce between them, but we choose just to
       throw an error instead. *)
    | RecLit _ -> error "ambiguous record literal"
    | RecUnit -> error "ambiguous unit record"

    (* Singleton types *)
    | SingType (ty, sing_tm) ->
        let ty = check context ty Semantics.Univ in
        let sing_tm = check context sing_tm (eval context ty) in
        (Syntax.SingType (ty, sing_tm), Semantics.Univ)

    (* Application *)
    | App (head, args) ->
        List.fold_left
          (fun (head, head_ty) arg ->
            match elim_implicits context head head_ty with
            | head, Semantics.FunType (_, param_ty, body_ty) ->
                let arg = check context arg param_ty in
                (Syntax.FunApp (head, arg), body_ty (eval context arg))
            | _ -> error "not a function")
          (infer context head)
          args

    (* Field projection *)
    | Proj (head, labels) ->
        List.fold_left
          (fun (head, head_ty) label ->
            match elim_implicits context head head_ty with
            | head, Semantics.RecType decls ->
                begin match Semantics.proj_ty (eval context head) decls label with
                | Some ty -> (Syntax.RecProj (head, label), ty)
                | None -> error ("field with label `" ^ label ^ "` not found in record")
                end
            | _ -> error "not a record")
          (infer context head)
          labels

    (* Patches. Here we add patches to record types by creating a copy of the
       type with singletons in place of the patched fields  *)
    | Patch (head, patches) ->
        let rec go context decls patches =
          match decls, patches with
          | Semantics.Nil, [] -> Syntax.Nil
          | Semantics.Nil, (label, _) :: _ ->
              error ("field `" ^ label ^ "` not found in record type")
          | Semantics.Cons (label, ty, tys), patches ->
              let ty' = quote context ty Univ in
              begin match List.assoc_opt label patches with
              | Some patch_tm ->
                  let tm = check context patch_tm ty in
                  let tm' = eval context tm in
                  let context = bind_def context label (Semantics.SingType (ty, tm')) tm' in
                  let patches = List.remove_assoc label patches in
                  Syntax.Cons (label, Syntax.SingType (ty', tm), go context (tys tm') patches)
              | None ->
                  let var = next_var context in
                  let context = bind_def context label ty var in
                  Syntax.Cons (label, ty', go context (tys var) patches)
              end
        in

        let dupes = List.find_dupes (List.map fst patches) in
        if List.compare_length_with dupes 0 <> 0 then
          error ("duplicate labels in patches: `" ^ String.concat "`, `" dupes ^ "`")
        else
          let head = check context head Semantics.Univ in
          begin match eval context head with
          | Semantics.RecType decls ->
              let decls = go context decls patches in
              (Syntax.RecType decls, Semantics.Univ)
          | _ -> error "can only patch record types"
          end

  (** {2 Eliminating implicit connectives} *)

  (** Connectives that were introduced implicitly during elaboration can
      sometimes get in the way, for example when calling {!coerce}, or when
      elaborating the head of an elimination. This removes them by adding
      appropriate elimination forms. *)
  and elim_implicits context tm = function
    (* Convert the singleton back to its underlying term using {!Syntax.SingElim} *)
    | Semantics.SingType (ty, sing_tm) ->
        let tm = Syntax.SingElim (tm, quote context sing_tm ty) in
        elim_implicits context tm ty
    (* TODO: we can eliminate implicit functions here. See the elaboration-zoo
      for ideas on how to do this: https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L48-L53 *)
    | ty -> (tm, ty)

end
