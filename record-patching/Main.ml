(** {0 Elaboration with Record Patching and Singleton Types}

    This is a small implementation of a dependently typed language with
    dependent record types, with some additional features intended to make it
    more convenient to use records as first-class modules. It was originally
    ported from {{: https://gist.github.com/mb64/04315edd1a8b1b2c2e5bd38071ff66b5}
    a gist by mb64}.

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


    (** {1 Pretty printing} *)

    let pretty names tm =
      let concat = String.concat "" in
      let parens wrap s = if wrap then concat ["("; s; ")"] else s in
      let rec go wrap names = function
        | Let (name, def, body) ->
            parens wrap (concat ["let "; name; " := "; go false names def; "; ";
              go false (name :: names) body])
        | Var index -> List.nth names index
        | Ann (tm, ty) -> parens wrap (concat [go true names tm; " : "; go false names ty])
        | Univ -> "Type"
        | FunType (name, param_ty, body_ty) ->
            parens wrap (concat ["fun ("; name; " : "; go false names param_ty; ") -> ";
              go false (name :: names) body_ty])
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
      and go_decls names decls =
        match decls with
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

  (** The initial elaboration context *)
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
  let quote_pretty context tm ty : string =
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
          let expected = quote_pretty context sing_tm to_ty in
          let found = quote_pretty context tm' from_ty in
          let ty = quote_pretty context to_ty Semantics.Univ in
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
        let expected = quote_pretty context to_ty Semantics.Univ in
        let found = quote_pretty context from_ty Semantics.Univ in
        error ("type mismatch: expected `" ^ expected ^ "`, found `" ^ found ^ "`")


  (** {2 Bidirectional type checking} *)

  (** Elaborate a term in the surface language into a term in the core language
      in the presence of a type annotation. *)
  let rec check context tm ty : Syntax.tm =
    match tm, ty with
    | Let (name, def_ty, def, body), ty ->
        begin match def_ty with
        | Some def_ty ->
            let def_ty = check context def_ty Semantics.Univ in
            let def_ty' = eval context def_ty in
            let def = check context def def_ty' in
            let context = bind_def context name def_ty' (eval context def) in
            Syntax.Let (name, Ann (def, def_ty), check context body ty)
        | None ->
            let def, def_ty = infer context def in
            let context = bind_def context name def_ty (eval context def) in
            Syntax.Let (name, def, check context body ty)
        end
    | FunLit (names, body), ty ->
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
    | RecUnit, Semantics.Univ ->
        Syntax.RecType Syntax.Nil
    | RecUnit, Semantics.RecType Semantics.Nil ->
        Syntax.RecLit []
    | tm, Semantics.SingType (ty, sing_tm) ->
        let tm = check context tm ty in
        let tm' = eval context tm in
        if is_convertible context sing_tm tm' ty then Syntax.SingIntro tm else
          let expected = quote_pretty context sing_tm ty in
          let found = quote_pretty context tm' ty in
          let ty = quote_pretty context ty Semantics.Univ in
          error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`")
    | tm, ty ->
        let tm, ty' = infer context tm in
        let tm, ty' = elim_implicits context tm ty' in
        coerce context tm ty' ty

  (** Elaborate a term in the surface language into a term in the core language,
      inferring its type. *)
  and infer context : tm -> Syntax.tm * Semantics.ty = function
    | Let (name, def_ty, def, body) ->
        begin match def_ty with
        | Some def_ty ->
            let def_ty = check context def_ty Semantics.Univ in
            let def_ty' = eval context def_ty in
            let def = check context def def_ty' in
            let context = bind_def context name def_ty' (eval context def) in
            let body, body_ty = infer context body in
            (Syntax.Let (name, Ann (def, def_ty), body), body_ty)
        | None ->
            let def, def_ty = infer context def in
            let context = bind_def context name def_ty (eval context def) in
            let body, body_ty = infer context body in
            (Syntax.Let (name, def, body), body_ty)
        end
    | Name name ->
        begin match List.elem_index name context.names with
        | Some index -> (Syntax.Var index, List.nth context.tys index)
        | None -> error ("`" ^ name ^ "` is not bound in the current scope")
        end
    | Ann (tm, ty) ->
        let ty = check context ty Semantics.Univ in
        let ty' = eval context ty in
        let tm = check context tm ty' in
        (Syntax.Ann (tm, ty), ty')
    | Univ ->
        (Syntax.Univ, Semantics.Univ)
    | FunType (params, body_ty) ->
        let rec go context = function
          | [] -> check context body_ty Semantics.Univ
          | (name, param_ty) :: params ->
              let param_ty = check context param_ty Semantics.Univ in
              let context = bind_param context name (eval context param_ty) in
              Syntax.FunType (name, param_ty, go context params)
        in
        (go context params, Semantics.Univ)
    | FunArrow (param_ty, body_ty) ->
        let param_ty = check context param_ty Semantics.Univ in
        let context = bind_param context "_" (eval context param_ty) in
        let body_ty = check context body_ty Semantics.Univ in
        (Syntax.FunType ("_", param_ty, body_ty), Semantics.Univ)
    | FunLit (_, _) -> error "ambiguous function literal"
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
    | RecLit _ -> error "ambiguous record literal"
    | RecUnit -> error "ambiguous unit record"
    | SingType (ty, sing_tm) ->
        let ty = check context ty Semantics.Univ in
        let sing_tm = check context sing_tm (eval context ty) in
        (Syntax.SingType (ty, sing_tm), Semantics.Univ)
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

(** Example terms for testing *)
module Examples = struct

  open Surface

  (* TODO: Implement a parser and convert to promote tests *)

  (*
    let F := {
      A : Type;
      B : Type;
      f : A -> B;
    };

    let record-ty-patch-1 := (fun x := x) : F [ B := A ] -> F;
    let record-ty-patch-2 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F;
    let record-ty-patch-3 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A; f := fun x := x ] -> F;
    let record-ty-patch-3b := (fun A x := x) : fun (A : Type) -> F [ B := A; f := fun x := x; A := A; ] -> F;
    let record-ty-patch-4 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ B := A ];
    let record-ty-patch-5 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ A := A ];
    let record-ty-patch-6 := (fun C x := x) : fun (C : Type) -> F [ A := C; B := C ] -> F [ B := C ];

    let record-lit-missing-1 := (fun C := { f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ];
    let record-lit-missing-2 := (fun C := {}) : fun (C : Type) -> F [ A := C; B := C; f := fun x := x ];
    let record-lit-missing-3 := (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ];

    let record-lit-coerce-1 :=
      (fun B r := r) :
        fun (B : Type) (r : { A : Type [= B ]; a : B })
          -> { A : Type; a : A };
    let record-lit-coerce-2 :=
      (fun B b := { A := B; a := b } : { A : Type; a : B }) :
        fun (B : Type) (b : Type) -> { A : Type; a : A };

    let record-lit-coerce-missing-1 := (fun A B r := r) : fun (A : Type) (B : Type) -> { f : A -> B } -> F [ A := A; B := B ];
    let record-lit-coerce-missing-2 := (fun A B r := r) : fun (A : Type) (B : Type) -> { A : Type; f : A -> B } -> F [ B := B ];

    let intro-sing := (fun A x := x) : fun (A : Type) (x : A) -> A [= x ];
    let elim-sing := (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [= x ]) -> A;

    let sing-tm-1 :=
      (fun A P f prf := prf) :
        fun (A : Type)
            (P : (fun (x : A) -> A [= x ]) -> Type)
            (f : fun (x : A) -> A [= x ])
            (prf : P (fun x := x))
        -> P f;

    let let-ann-check :=
      (let id : fun (A : Type) -> A -> A :=
        fun A a := a; {}) : Type;

    let let-ann-synth :=
      let id : fun (A : Type) -> A -> A :=
        fun A a := a;
      id {} {};

    -- Example from page 4 of “1ML – Core and Modules United”
    let map-functor :=

      let Bool := fun (Out : Type) { true : Out; false : Out } -> Out;
      let true : Bool := fun Out cases := cases.true;
      let false : Bool := fun Out cases := cases.false;

      let Option : Type -> Type := fun A :=
        fun (Out : Type) { some : A -> Out; none : Out } -> Out;

      let none : fun (A : Type) -> Option A :=
        fun A := fun Out cases := cases.none;
      let some : fun (A : Type) -> A -> Option A :=
        fun A a := fun Out cases := cases.some a;

      let Eq := {
        T : Type;
        eq : T -> T -> Bool;
      };

      let Map := {
        Key : Type;
        Map : Type -> Type;
        empty : fun (A : Type) -> Map A;
        add : fun (A : Type) -> Key -> A -> Map A -> Map A;
        lookup : fun (A : Type) -> Key -> Map A -> Option A;
      };

      -- TODO: sealing operator
      let eq-map : fun (key : Eq) -> Map [ Key := key.T ] :=
        fun key := {
          Map := fun A := key.T -> Option A
          empty := fun A := fun x := none A;
          add := fun A k v map :=
            fun x := (key.eq x k) (Option A) {
              true := some A v;
              false := map x;
            };
          lookup := fun A k map := map k;
        };

      Type;

    -- TODO: requires total space conversion like in CoolTT

    let category := {
      Ob : Type;
      Hom : { s : Ob; t : Ob } -> Type;
      id : fun (x : Ob) -> Hom [ s := x; t := x ];
      seq : fun (f : Hom) (g : Hom [ s := f.t ]) -> Hom [ s := f.s; t := g.t ];
    };

    let types : category := {
      Ob := Type;
      Hom := fun params := params.s -> params.t;
      id := fun A a := a;
      seq := fun f g a := g (f a);
    };

    Type
  *)

  (* let F := { A : Type; B : Type; f : A -> B }; *)
  let fun_record_ty =
    RecType [
      "A", Univ;
      "B", Univ;
      "f", FunArrow (Name "A", Name "B");
    ]

  let record_ty_patch1 =
    Let ("F", None, fun_record_ty,
      (* (fun x := x) : F [ B := A ] -> F *)
      Ann (FunLit (["x"], Name "x"),
        FunArrow (Patch (Name "F", ["B", Name "A"]), Name "F")))

  let record_ty_patch2 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]), Name "F"))))

  let record_ty_patch3 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A; f := fun x := x ] -> F *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"; "f", FunLit (["x"], Name "x")]),
            Name "F"))))

  let record_ty_patch3b =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ B := A; f := fun x := x; A := A; ] -> F; *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["B", Name "A"; "f", FunLit (["x"], Name "x"); "A", Name "A"]),
            Name "F"))))

  let record_ty_patch4 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ B := A ] *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
            Patch (Name "F", ["B", Name "A"])))))

  let record_ty_patch5 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ A := A ] *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
            Patch (Name "F", ["A", Name "A"])))))

  let record_ty_patch6 =
    Let ("F", None, fun_record_ty,
      (* (fun C x := x) : fun (C : Type) -> F [ A := C; B := C ] -> F [ B := C ] *)
      Ann (FunLit (["C"; "x"], Name "x"),
        FunType (["C", Univ],
          FunArrow (Patch (Name "F", ["A", Name "C"; "B", Name "C"]),
            Patch (Name "F", ["B", Name "C"])))))

  let record_lit_missing1 =
    Let ("F", None, fun_record_ty,
      (* (fun C := { f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ] *)
      Ann (FunLit (["C"], RecLit ["f", FunLit (["x"], Name "x")]),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  let record_lit_missing2 =
    Let ("F", None, fun_record_ty,
      (* (fun C := {}) : fun (C : Type) -> F [ A := C; B := C; f := fun x := x ] *)
      Ann (FunLit (["C"], RecLit []),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"; "f", FunLit (["x"], Name "x")]))))

  let record_lit_missing3 =
    Let ("F", None, fun_record_ty,
      (* (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ] *)
      Ann (FunLit (["C"], RecLit ["B", Name "C"; "f", FunLit (["x"], Name "x")]),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  (*
    (fun B r := r) :
      fun (B : Type) (r : { A : Type [= B ]; a : B })
        -> { A : Type; a : A }
  *)
  let record_lit_coerce1 =
    Ann (FunLit (["B"; "r"], Name "r"),
      FunType (["B", Univ; "r", RecType ["A", SingType (Univ, Name "B"); "a", Name "B"]],
        RecType ["A", Univ; "a", Name "A"]))

  (*
    (fun B b := { A := B; a := b } : { A : Type; a : B }) :
      fun (B : Type) (b : Type) -> { A : Type; a : A }
  *)
  let record_lit_coerce2 =
    Ann (FunLit (["B"; "b"],
        Ann (RecLit ["A", Name "B"; "a", Name "b"],
          RecType ["A", Univ; "a", Name "B"])),
      FunType (["B", Univ; "b", Name "B"], RecType ["A", Univ; "a", Name "A"]))

  (*
    (fun A B r := r) : fun (A : Type) (B : Type) -> { f : A -> B } -> F [ A := A; B := B ]
  *)
  let record_lit_coerce_missing1 =
    (* let F := { A : Type; B : Type; f : A -> B }; *)
    Let ("F", None, fun_record_ty,
      Ann (FunLit (["A"; "B"; "r"], Name "r"),
        FunType (["A", Univ; "B", Univ], FunArrow (RecType ["f", FunArrow (Name "A", Name "B")],
          Patch (Name "F", ["A", Name "A"; "B", Name "B"])))))

  (*
    (fun B r := r) : fun (B : Type) -> { A : Type; f : A -> B } -> F [ B := B ]
  *)
  let record_lit_coerce_missing2 =
    (* let F := { A : Type; B : Type; f : A -> B }; *)
    Let ("F", None, fun_record_ty,
      Ann (FunLit (["B"; "r"], Name "r"),
        FunType (["B", Univ], FunArrow (RecType ["A", Univ; "f", FunArrow (Name "A", Name "B")],
          Patch (Name "F", ["B", Name "B"])))))

  (*
     (fun A x := x) : fun (A : Type) (x : A) -> A [= x ]
  *)
  let intro_sing =
    Ann (FunLit (["A"; "x"], Name "x"),
      FunType (["A", Univ; "x", Name "A"], SingType (Name "A", Name "x")))

  (*
     (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [= x ]) -> A
  *)
  let elim_sing =
    Ann (FunLit (["A"; "x"; "sing-x"], Name "sing-x"),
      FunType (["A", Univ; "x", Name "A"; "sing-x", SingType (Name "A", Name "x")], Name "A"))

  (*
    (fun A P f pf := pf) :
      fun (A : Type)
          (P : (fun (x : A) -> A [= x ]) -> Type)
          (f : fun (x : A) -> A [= x ])
          (pf : P (fun x := x))
      -> P f
  *)
  let sing_tm1 =
    Ann (FunLit (["A"; "P"; "f"; "prf"], Name "prf"),
      FunType ([
        "A", Univ;
        "P", FunType (["_", FunType (["x", Name "A"], SingType (Name "A", Name "x"))], Univ);
        "f", FunType (["x", Name "A"], SingType (Name "A", Name "x"));
        "prf", App (Name "P", [FunLit (["x"], Name "x")]);
      ], App (Name "P", [Name "f"])))

  (*
    (let id : fun (A : Type) -> A -> A :=
      fun A a := a; {}) : Type
  *)
  let let_ann_check =
    Ann (Let ("id", Some (FunType (["A", Univ], FunArrow (Name "A", Name "A"))), FunLit (["A"; "a"], Name "a"),
      RecUnit), Univ)

  (*
    let id : fun (A : Type) -> A -> A :=
      fun A a := a;
    id {} {}
  *)
  let let_ann_synth =
    Let ("id", Some (FunType (["A", Univ], FunArrow (Name "A", Name "A"))), FunLit (["A"; "a"], Name "a"),
      App (Name "id", [RecUnit; RecUnit]))

  (*
    -- Example from page 4 of “1ML – Core and Modules United”

    let Bool := fun (Out : Type) (cases : { true : Out; false : Out }) -> Out;
    let true : Bool := fun Out cases := cases.true;
    let false : Bool := fun Out cases := cases.false;

    let Option : Type -> Type := fun A :=
      fun (Out : Type) (cases : { some : A -> Out; none : Out }) -> Out;

    let none : fun (A : Type) -> Option A :=
      fun A := fun Out cases := cases.none;
    let some : fun (A : Type) -> A -> Option A :=
      fun A a := fun Out cases := cases.some a;

    let Eq := {
      T : Type;
      eq : T -> T -> Bool;
    };

    let Map := {
      Key : Type;
      Map : Type -> Type;
      empty : fun (A : Type) -> Map A;
      add : fun (A : Type) -> Key -> A -> Map A -> Map A;
      lookup : fun (A : Type) -> Key -> Map A -> Option A;
    };

    -- TODO: sealing operator
    let eq-map : fun (key : Eq) -> Map [ Key := key.T ] :=
      fun key := {
        Map := fun A := key.T -> Option A
        empty := fun A := fun x := none A;
        add := fun A k v map :=
          fun x := (key.eq x k) (Option A) {
            true := some A v;
            false := map x;
          };
        lookup := fun A k map := map k;
      };

    Type
  *)
  let map_functor =
    Let ("Bool", None, FunType (["Out", Univ; "cases", RecType ["true", Name "Out"; "false", Name "Out"]], Name "Out"),
    Let ("true", Some (Name "Bool"), FunLit (["Out"; "cases"], Proj (Name "cases", ["true"])),
    Let ("false", Some (Name "Bool"), FunLit (["Out"; "cases"], Proj (Name "cases", ["false"])),

    Let ("Option", Some (FunArrow (Univ, Univ)),
      FunLit (["A"], FunType (["Out", Univ; "cases", RecType ["some", FunArrow (Name "A", Name "Out"); "none", Name "Out"]], Name "Out")),
    Let ("none", Some (FunType (["A", Univ], App (Name "Option", [Name "A"]))),
      FunLit (["A"], FunLit (["Out"; "cases"], Proj (Name "cases", ["none"]))),
    Let ("some", Some (FunType (["A", Univ], FunArrow (Name "A", App (Name "Option", [Name "A"])))),
      FunLit (["A"; "a"], FunLit (["Out"; "cases"], App (Proj (Name "cases", ["some"]), [Name "a"]))),

    Let ("Eq", None,
      RecType ["T", Univ; "eq", FunArrow (Name "T", FunArrow (Name "T", Name "Bool"))],
    Let ("Map", None,
      RecType [
        "Key", Univ;
        "Map", FunArrow (Univ, Univ);
        "empty", FunType (["A", Univ], App (Name "Map", [Name "A"]));
        "add", FunType (["A", Univ],
          FunArrow (Name "Key", FunArrow (Name "A",
            FunArrow (App (Name "Map", [Name "A"]), App (Name "Map", [Name "A"])))));
        "lookup", FunType (["A", Univ],
          FunArrow (Name "Key",
            FunArrow (App (Name "Map", [Name "A"]), App (Name "Option", [Name "A"]))));
      ],
    Let ("eq-map", Some (FunType (["key", Name "Eq"], Patch (Name "Map", ["Key", Proj (Name "key", ["T"])]))),
      FunLit (["key"],
        RecLit [
          "Map", FunLit (["A"], FunArrow (Proj (Name "key", ["T"]), App (Name "Option", [Name "A"])));
          "empty", FunLit (["A"], FunLit (["k"], App (Name "none", [Name "A"])));
          "add", FunLit (["A"; "k"; "v"; "map"],
            FunLit (["x"], App (App (Proj (Name "key", ["eq"]), [Name "x"; Name "k"]), [
              App (Name "Option", [Name "A"]);
              RecLit [
                "true", App (Name "some", [Name "A"; Name "v"]);
                "false", App (Name "none", [Name "A"]);
              ];
            ])));
          "lookup", FunLit (["A"; "k"; "map"], App (Name "map", [Name "k"]));
        ]),
      Univ)))))))))

  (*
    let category := {
      Ob : Type;
      Hom : { s : Ob; t : Ob } -> Type;
      id : fun (x : Ob) -> Hom [ s := x; t := x ];
      seq : fun (f : Hom) (g : Hom [ s := f.t }) -> Hom [ s := f.s; t := g.t ];
    };
  *)
  let category_ty =
    RecType [
      "Ob", Univ;
      "Hom", FunArrow (RecType ["s", Name "Ob"; "t", Name "Ob"], Univ);
      "id", FunType (["x", Name "Ob"], Patch (Name "Hom", ["s", Name "x"; "t", Name "x"]));
      "seq", FunType (["f", Name "Hom"; "g", Patch (Name "Hom", ["s", Proj (Name "f", ["t"])])],
        Patch (Name "Hom", ["s", Proj (Name "f", ["s"]); "t", Proj (Name "g", ["t"])]));
    ]

  (*
    let types : category := {
      Ob := Type;
      Hom := fun params := params.s -> params.t;
      id := fun A a := a;
      seq := fun f g a := g (f a);
    };
  *)
  let types_tm =
    Let ("category", None, category_ty,
      Ann (
        RecLit [
          "Ob", Univ;
          "Hom", FunLit (["params"], FunArrow (Proj (Name "params", ["s"]), Proj (Name "params", ["t"])));
          "id", FunLit (["A"; "a"], Name "a");
          "seq", FunLit (["f"; "g"; "a"], App (Name "g", [Name "f"; Name "a"]));
        ],
        Name "category"))

  let terms = [
    "fun_record_ty", fun_record_ty;
    "record_ty_patch1", record_ty_patch1;
    "record_ty_patch2", record_ty_patch2;
    "record_ty_patch3", record_ty_patch3;
    "record_ty_patch3b", record_ty_patch3b;
    "record_ty_patch4", record_ty_patch4;
    "record_ty_patch5", record_ty_patch5;
    "record_ty_patch6", record_ty_patch6;
    "record_lit_missing1", record_lit_missing1;
    "record_lit_missing2", record_lit_missing2;
    "record_lit_missing3", record_lit_missing3;
    "record_lit_coerce1", record_lit_coerce1;
    "record_lit_coerce2", record_lit_coerce2;
    "record_lit_coerce_missing1", record_lit_coerce_missing1;
    "record_lit_coerce_missing2", record_lit_coerce_missing2;
    "intro_sing", intro_sing;
    "elim_sing", elim_sing;
    "sing_tm1", sing_tm1;
    "let_ann_check", let_ann_check;
    "let_ann_synth", let_ann_synth;
    "map_functor", map_functor;
    (* TODO: requires total space conversion like in CoolTT *)
    (* "category_ty", category_ty; *)
    (* "types_tm", types_tm; *)
  ]

end

let () =
  Printexc.record_backtrace true;

  let results = Examples.terms |> List.map
    (fun (name, term) ->
      Format.printf "testing %s:\n" name;
      Format.printf "\n";
      try
        let context = Surface.initial_context in
        let tm, ty = Surface.infer context term in
        Format.printf "  inferred type   │ %s\n" (Surface.quote_pretty context ty Core.Semantics.Univ);
        Format.printf "  evaluated term  │ %s\n" (Surface.quote_pretty context (Surface.eval context tm) ty);
        Format.printf "\n";
        Format.printf "  %s ... ok\n" name;
        Format.printf "\n";
        (name, `Passed)
      with e ->
        let msg = Printexc.to_string e in
        let stack = Printexc.get_backtrace () |> String.split_on_char '\n' in
        Format.printf "  caught exception: \n";
        Format.printf "\n";
        List.iter (fun line -> Format.printf "    %s\n" line) (msg :: stack);
        Format.printf "  %s ... FAILED\n" name;
        Format.printf "\n";
        (name, `Failed)
    ) in

  let passed, failed, name_width = results |> List.fold_left
    (fun (passed, failed, width) -> function
      | name, `Passed -> (passed + 1, failed, max (String.length name) width)
      | name, `Failed -> (passed, failed +1, max (String.length name) width))
    (0, 0, 0) in

  Format.printf "\n";
  Format.printf "test summary:\n";
  Format.printf "\n";
  results |> List.iter (function
    | (name, `Passed) -> Format.printf "  %-*s ... ok\n" name_width name
    | (name, `Failed) -> Format.printf "  %-*s ... FAILED\n" name_width name);
  Format.printf "\n";
  Format.printf "test result: %s. %i passed; %i failed\n"
    (if failed = 0 then "ok" else "FAILED") passed failed;

  exit (if failed = 0 then 0 else 1)


(** {1 Syntax bikeshedding}

    {2 Record patching}

    - [ R [ B := A; ... ] ]
    - [ R.{ B := A; ... } ] (better for postfix chaining)
    - [ R # [ B .= A, ... ] ] (like in CoolTT)
    - [ R # { B := A; ... } ]
    - [ R (B := A, ...) ] (possibly overloaded with function application)
    - [ R where B := A, ... ] (like in Standard-ML)

    {2 Singleton types}

    - [ A [ x ] ] (like in mb64’s original implementation)
    - [ A [= x ] ] (riffing on the idea of using square brackets for ‘refinement’)
    - [ A [:= x ] ] (mirrors patching more)
    - [ (= x) ] (like in 1ML)
    - [ (= x : A) ]
    - [ (:= x : A) ]
    - [ A (= x) ]
    - [ A (:= x) ] (parens read like, “btw, it's equal to [ x ]”)
*)
