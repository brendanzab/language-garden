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

    Adding a ‘sealing operator’ [ e :> t ] to the surface language would allow
    us to opaquely ascribe type type [ t ] to an expression [ e ]. This would
    prevent the contents of the expression from reducing definitionally,
    allowing us to define abstract data types.

    Opaque ascription is sometimes modelled using effects, as seen in th
    language {{:https://people.mpi-sws.org/~rossberg/1ml/} 1ML}. The paper
    {{:https://doi.org/10.1145/3474834} “Logical Relations as Types:
    Proof-Relevant Parametricity for Program Modules”} describes an effectful
    approach based on call-by-push-value that could be useful in the context of
    dependent types. That said, apparently a effects are only needed in the
    presence of mutable references. If we didn’t need these, we might be able
    implement sealing by hiding definitions behind function parameters. For
    example:

    - [ ... (e :> t) ... ] could elaborate into [ (fun (x : t) := ... x ...) e ]
    - [ let x :> t := e; ... ] could elaborate into [ (fun (x : t) := ...) e ].

    More concretetly, the following:

    {[
      ({ A := Nat, a := 0 } :>
        { A : Type, a : T }).a + 1 ...
                          -- ^ error: expected `Nat` found `(..).A`
    ]}

    Could be elaborated into:

    {[
      fun (x : { A : Type, a : T }) :=
        x.a + 1 ...) { A := Nat, a := 0 }
       -- ^ error: expected `Nat` found `x.A`
    ]}

    {2 Metavariables and unification}

    Implicit function types and unification could be convenient. This could be
    challenging to implement in the presence coercive subtyping, however.
    Apparently total space conversion addresses some of the same pain points as
    implicit parameters, but I'm still somewhat skeptical of this!

    {2 Reduce patch bloat}

    Each patch currently elaborates to a copy of the original record type. This
    could end up becoming a performance issue when elaborating and compiling
    larger programs.

    {2 Use patch syntax for record literal updates}

    The same syntax used by patches could be used as a way to update the fields
    of record literals.
*)

(** Returns the index of the given element in the list *)
let elem_index a =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = a then Some i else go (i + 1) xs in
  go 0

(** Core language *)
module Core = struct
  (** In this module we define a core language that is intended to be simple,
      explicit and relatively close to well-understood type theories. *)


  (** {1 Names} *)

  (** Labels are significant to the equality of terms. They are typically used
      in the fields of records, and in record projections. *)
  type label = string

  (** Names that serve as hints when pretty printing binders and variables, but
      should not impact the equality of terms. *)
  type name = string


  (** Core Syntax *)
  module Syntax = struct

    (** De-bruijn index *)
    type index = int

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

  end

  (** Semantics of the core language *)
  module Semantics = struct
    (** In order to type check our language, it’s important to be able to check
        if two terms compute to the same thing (eg. when checking if two types
        are the same), or to perform some computation in order to uncover
        details about terms (eg. to figure out if a type is a function type).
        Terms could contain free variables (eg. when computing a variable that
        is bound to a function parameter) so computation might get stuck at
        various points. We also want to take a lazy approach, only computing
        as much as we need.

        To meet these goals, we implement an interpreter using {i normalisation
        by evaluation}, which involves first evaluating terms from our syntax
        into partially reduced terms in the {i semantic  domain}, then either
        those terms back to the original syntax to arrive at normal forms, or
        comparing them using conversion checking.
    *)


    (** {1 Semantic domain} *)

    (** The following data structures represent the semantic interpretation of
        the core syntax. *)

    (** De-bruijn level *)
    type level = int

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

    (** Neutral terms

        These are terms that could not be reduced to a normal form as a result
        of being stuck on something else that would not reduce further.

        I’m not sure why they are called ‘neutral terms’. Perhaps they are...
        ambivalent about what they might evaluate to?
    *)
    and neu =
      | Var of level                        (** Variable that could not be reduced further *)
      | FunApp of neu * tm                  (** Function application *)
      | RecProj of neu * label              (** Record projection *)

    (** An environment of bound entries that can be looked up directly using a
        {!Syntax.index}, or by inverting a {!level} using [ size - level - 1 ],
        where [ size ] is the number of entries bound in the environment. *)
    type 'a env = 'a list


    (** {1 Error handling} *)

    (** Internal error encountered in the semantics. These should never occur if
        terms are used in a way that respects the type system of the core
        language. *)
    exception Error of string


    (** {1 Eliminators} *)

    (** The following functions trigger computation if the head term is in the
        appropriate normal form, otherwise they queues up the elimination for
        later if computation if the term is in a neutral form. *)

    (** Compute a function application *)
    let app : tm -> tm -> tm = function
      | FunLit (_, body) -> body
      | Neu neu -> fun arg -> Neu (FunApp (neu, arg))
      | _ -> raise (Error "invalid app")

    (** Compute a record projection *)
    let proj : tm -> label -> tm = function
      | RecLit defns -> fun label -> defns |> List.find (fun (l, _) -> l = label) |> snd
      | Neu neu -> fun label -> Neu (RecProj (neu, label))
      | _ -> raise (Error "invalid proj")


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

    (** Quotation allows us to turn the semantic domain back into syntax. This
        is useful if we want to find the normal form of a term, or if we want to
        include a semantic term in some other bit of syntax.

        Quotation is type directed, but we only use types as a way to restore
        the values of singletons that were erased during evaluation.

        The size parameter is the number of bindings in the environment where we
        want quotation to occur. This allows us to convert variables from
        de-Bruijn levels back to de-Bruijn indices. We need to be careful to
        only use the resulting terms at binding depth that they were quoted at.
        Note that we could alternatively compute this from the length of the
        typing environment, but this would be an O(n) operation whenever we
        wanted to convert between variable representations.
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
          | _ -> raise (Error "not a function type")
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
                | _, _ -> raise (Error "mismatched fields")
              in
              Syntax.RecLit (go defns decls)
          | _ -> raise (Error "not a record type")
          end
      | SingType (ty, sing_tm) ->
          Syntax.SingType (quote size tys ty Univ, quote size tys sing_tm ty)
      | SingIntro ->
          begin match ty with
          (* Restore the erased term from the singleton type *)
          | SingType (ty, sing_tm) -> Syntax.SingIntro (quote size tys ty sing_tm)
          | _ -> raise (Error "not a singleton type")
          end
    and quote_neu size tys : neu -> Syntax.tm * ty = function
      | Var level ->
          let index = size - level - 1 in
          (Syntax.Var index, List.nth tys index)
      | FunApp (head, arg) ->
          begin match quote_neu size tys head with
          | head, FunType (_, param_ty, body_ty) ->
              (Syntax.FunApp (head, (quote size tys arg param_ty)), body_ty arg)
          | _ -> raise (Error "not a function type")
          end
      | RecProj (head, label) ->
          begin match quote_neu size tys head with
          | head', RecType decls ->
              let ty = proj_ty (Neu head) decls label |> Option.get in
              (Syntax.RecProj (head', label), ty)
          | _ -> raise (Error "not a record type")
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

    (** Conversion checking is checks if two terms of the same type compute to
        the same term by-definition.

        Type-directed conversion allows us to support full eta for unit types,
        which show up in our language as empty records and singletons. If we
        wanted to stick to untyped conversion checking, according to Andras
        Korvacs we could alternatively:

        - perform best-effort eta, where unit elements are the same as anything
        - detect definitionally irrelevant types during elaboration, marking
          irrelevant terms
    *)
    let rec is_convertible size tys tm1 tm2 : ty -> bool = function
      | Neu _ ->
          begin match tm1, tm2 with
          | Neu n1, Neu n2 -> Option.is_some (is_convertible_neu size tys n1 n2)
          | _, _ -> raise (Error "internal error") (* why? *)
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
      | _ -> raise (Error "not a type")
    and is_convertible_neu size tys neu1 neu2 =
      match neu1, neu2 with
      | Var level1, Var level2 when level1 = level2 -> Some (List.nth tys (size - level1 - 1))
      | FunApp (func1, arg1), FunApp (func2, arg2) ->
          begin match is_convertible_neu size tys func1 func2 with
          | Some (FunType (_, param_ty, body_ty)) ->
              if is_convertible size tys arg1 arg2 param_ty then Some (body_ty arg1) else None
          | Some _ -> raise (Error "not a function type")
          | None -> None
          end
      | RecProj (record1, label1), RecProj (record2, label2) when label1 = label2 ->
          begin match is_convertible_neu size tys record1 record2 with
          | Some (RecType decls) -> proj_ty (Neu record1) decls label1
          | Some _ -> raise (Error "not a record type")
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


    (** {1 Pretty printing} *)

    (** Rough-and-ready pretty printer *)
    let pretty size names tm =
      let (<<) f g x = f (g x) in
      let str s rest = s ^ rest in
      let parens wrap s rest = if wrap then "(" ^ s (")" ^ rest) else s rest in
      let rec go wrap size names = function
        | Neu neu -> go_neu wrap size names neu
        | Univ -> str "Type"
        | FunType (name, param_ty, body_ty) ->
            parens wrap (str "fun (" << str name << str " : " << go false size names param_ty <<
              str ") -> " << go false (size + 1) (name :: names) (body_ty (Neu (Var size))))
        | FunLit (name, body) ->
            parens wrap (str "fun " << str name << str " := " <<
              go false (size + 1) (name :: names) (body (Neu (Var size))))
        | RecType Nil | RecLit [] -> str "{}"
        | RecType decls ->
            str "{ " << go_decls size names decls << str "}"
        | RecLit defns ->
            str "{ " <<
              List.fold_right
                (fun (label, tm) rest ->
                  str label << str " := " << go false size names tm << str "; " << rest)
                defns (str "}")
        | SingType (ty, sing_tm) ->
            parens wrap (go false size names ty << str " [= " <<
              go false size names sing_tm << str " ]")
        | SingIntro -> str "#sing-intro"
      and go_neu wrap size names = function
        | Var level -> str (List.nth names (size - level - 1))
        | FunApp (head, arg) ->
            parens wrap (go_neu false size names head << str " " <<  go true size names arg)
        | RecProj (head, label) -> go_neu false size names head << str "." << str label
      and go_decls size names decls =
        match decls with
        | Nil -> str ""
        | Cons (label, ty, decls) ->
            str label << str " : " << go false size names ty << str "; " <<
              go_decls (size + 1) (label :: names) (decls (Neu (Var size)))
      in
      go false size names tm ""
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
    size : Semantics.level;             (** Number of entries bound. *)
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
  let pretty context : Semantics.tm -> string =
    Semantics.pretty context.size context.names


  (** {2 Elaboration errors} *)

  (** An elaboration error that will be raised if there was a problem in the
      surface syntax, usually as a result of type errors or a lack of type
      annotations. This is normal, and should be rendered nicely to the
      programmer. *)
  exception Error of string


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
          let expected = pretty context sing_tm in
          let found = pretty context tm' in
          let ty = pretty context to_ty in
          raise (Error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`"))
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
              raise (Error ("type mismatch: expected field `" ^ to_label ^ "`, found field `" ^ from_label ^ "`"))
          | _, _ -> raise (Semantics.Error "mismatched declsscope length")
        in
        Syntax.RecLit (go from_decls to_decls)
    (* TODO: subtyping for functions! *)
    | from_ty, to_ty  ->
        let expected = pretty context to_ty in
        let found = pretty context from_ty in
        raise (Error ("type mismatch: expected `" ^ expected ^ "`, found `" ^ found ^ "`"))


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
          | _, _ -> raise (Error "too many parameters in function literal")
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
          | _, Semantics.Cons (label, _, _) -> raise (Error ("field `" ^ label ^ "` is missing from record literal"))
          | (label, _) :: _, Semantics.Nil -> raise (Error ("unexpected field `" ^ label ^ "` in record literal"))
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
          let expected = pretty context sing_tm in
          let found = pretty context tm' in
          let ty = pretty context ty in
          raise (Error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`"))
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
        begin match elem_index name context.names with
        | Some index -> (Syntax.Var index, List.nth context.tys index)
        | None -> raise (Error ("`" ^ name ^ "` is not bound in the current scope"))
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
    | FunLit (_, _) ->
        raise (Error "ambiguous function literal")
    | RecType decls ->
        let rec go context seen_labels = function
          | [] -> (Syntax.Nil)
          | (label, _) :: _ when List.mem label seen_labels ->
              raise (Error ("duplicate label `" ^ label ^ "` in record type"))
          | (label, ty) :: decls ->
              let ty = check context ty Semantics.Univ in
              let context = bind_param context label (eval context ty) in
              Syntax.Cons (label, ty, go context (label :: seen_labels) decls)
        in
        (Syntax.RecType (go context [] decls), Semantics.Univ)
    | RecLit _ ->
        raise (Error "ambiguous record literal")
    | RecUnit ->
        raise (Error "ambiguous unit record")
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
            | _ -> raise (Error "not a function"))
          (infer context head)
          args
    | Proj (head, labels) ->
        List.fold_left
          (fun (head, head_ty) label ->
            match elim_implicits context head head_ty with
            | head, Semantics.RecType decls ->
                begin match Semantics.proj_ty (eval context head) decls label with
                | Some ty -> (Syntax.RecProj (head, label), ty)
                | None -> raise (Error ("field `" ^ label ^ "` not found in record"))
                end
            | _ -> raise (Error "not a record"))
          (infer context head)
          labels
    | Patch (head, patches) ->
        let rec go context decls patches =
          match decls, patches with
          | Semantics.Nil, [] -> Syntax.Nil
          | Semantics.Nil, (label, _) :: _ ->
              raise (Error ("field `" ^ label ^ "` not found in record type"))
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
        let patch_labels = List.map fst patches in
        if List.sort_uniq (String.compare) patch_labels <> patch_labels then
          raise (Error "duplicate field labels in patches")
        else
          let head = check context head Semantics.Univ in
          begin match eval context head with
          | Semantics.RecType decls ->
              let decls = go context decls patches in
              (Syntax.RecType decls, Semantics.Univ)
          | _ -> raise (Error "can only patch record types")
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
  let passed, failed = Examples.terms |> List.fold_left
    (fun (passed, failed) (name, term) ->
      print_endline ("testing " ^ name ^ ":");
      print_endline "";
      try
        let context = Surface.initial_context in
        let tm, ty = Surface.infer context term in
        print_endline ("  inferred type   │ " ^ Surface.pretty context ty);
        print_endline ("  evaluated term  │ " ^ Surface.pretty context (Surface.eval context tm));
        print_endline "";
        print_endline ("  " ^ name ^ ": ok.");
        print_endline "";
        (passed + 1, failed)
      with e ->
        let msg = Printexc.to_string e in
        let stack = Printexc.get_backtrace () in
        print_endline "  caught exception:";
        print_endline "";
        print_endline ("    " ^ msg);
        String.split_on_char '\n' stack
          |> List.iter (fun line -> print_endline ("    " ^ line));
        print_endline ("  " ^ name ^ ": error.");
        print_endline "";
        (passed, failed + 1)
    )
    (0, 0) in
  print_endline
    ("testing result: " ^ (if failed = 0 then "ok" else "error") ^ ". " ^
      string_of_int passed ^ " passed; " ^
      string_of_int failed ^ " failed")

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
