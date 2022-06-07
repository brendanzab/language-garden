(** {0 Elaboration with Record Patching and Singleton Types}

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

    This is similar to Standard ML’s [ where type ] syntax for {{: https://smlfamily.github.io/sml97-defn.pdf#page=28}
    type realisation}, OCaml’s [ with ] operator for {{: https://v2.ocaml.org/manual/modtypes.html#ss%3Amty-with}
    constraining module types}, and Rust’s [ Iterator<Item = T> ] shorthand
    syntax for {{: https://rust-lang.github.io/rfcs/0195-associated-items.html#constraining-associated-types}
    equality constraints} in type parameter bounds.

    Patches don’t need to be limited to types either. For example:

    {[
      let nat-mul-monoid : Monoid [ T := Nat ] := {
        empty := 1;
        append := fun x y := x * y;
      };

      let nat-mul-monoid-2 : Monoid [ T := Nat; empty := 1 ] :=
        nat-mul-monoid;
    ]}

    {1 Elaboration of patches}

    Patches only exist as a feature of the surface language and are removed
    during elaboration. The expression [ Monoid [ T := String ] ] in the example
    above elaborates to a new record type, where the type of the [ T ] field is
    constrained to [ String ] through the use of a singleton type.

    We also infer the definitions of fields from singletons. This works nicely
    in combination with record patching. Note how we don't need to mention the
    field [ T ] in the definition of [ string-monoid ].

    With that in mind, the definition of [ string-monoid ] is elaborated to:

    {[
      let string-monoid : {
        T : Type [= String ]; -- singleton type patched here
        empty : T;
        append : T -> T -> T;
      } := {
        T := String; -- definition inferred from the singleton
        empty := "";
        append := string-append;
      };
    ]}

    {1 References}

    - This implementation of record patching by elaborating to singletons is
      heavily based on {{: https://gist.github.com/mb64/04315edd1a8b1b2c2e5bd38071ff66b5}
      Mark Barbone’s implementation sketch in Haskell}, but contains some bug
      fixes, alterations, and extensions.
    - Record patching was originally proposed and implemented for CoolTT.
      - {{: https://github.com/RedPRL/cooltt/issues/266} Record patching (like
        SML [ where type ])}
      - {{: https://github.com/RedPRL/cooltt/issues/267} Support for auto-
        converting between fibered and parameterized type families}
      - Reed Mullanix's {{: https://www.youtube.com/watch?v=1_ZJIYu2BRk}
        presentation} and {{: https://cofree.coffee/~totbwf/slides/WITS-2022.pdf}
        slides} from WITS’22.

    {1 Future work}

    - Implement a parser for the surface language.
    - Experiment with adding ‘generativity’. This would allow the contents
      of a record to be hidden, allowing us to use them as a way to define
      abstract data types.
    - Figure out how to implement ‘total space conversion’, like in CoolTT. This
      automatically converts [ F : { l : T; ... } -> Type ] to the record type
      [ { l : T; ..., fibre : F { l := l; ... } } ] where necessary.
    - Attempt to implementing metavariables, unification, and implicit function
      types. This could be challenging in the presence subtyping, however. Total
      space conversion apparently makes implicit parameters less necessary, but
      I'm still a little skeptical of this!
    - Figure out ways to avoid the bloat that patches introduce (note how each
      patch elaborates to a copy of the original record type). This could become
      a perfomance issue if patching is used heavily.
*)

(** Returns the index of the given element in the list *)
let elem_index a =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = a then Some i else go (i + 1) xs in
  go 0

(** Core language *)
module Core = struct

  (** Identifiers that are significant to the equality of terms. Typically used
      in the fields of records, and in record projections. *)
  type label = string

  (** Identifiers that serve as hints when pretty printing binders and
      variables, but should not impact the equality of terms. *)
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
      | Ann of tm * ty
      | Var of index
      | Univ
      | FunType of name * ty * (ty binds)
      | FunLit of name * (tm binds)
      | FunApp of tm * tm
      | RecType of label list * tele
      | RecLit of (label * tm) list
      | RecProj of tm * label
      (* Singleton type former, eg. [ A [= x ] ].

        These types constrain a type [ A ] to be equal to a single value [ x ].
      *)
      | SingType of ty * tm
      (* Introduction form for singletons.

        This does not appear in user code, but is inserted during elaboration.
        For example, if [ x : Nat ] and [ x ≡ 7 ], using [ x ] as [ Nat [= 7 ] ]
        elaborates to [ SingIntro x ].
      *)
      | SingIntro of tm
      (* Elimination form for singletons.

        This does not appear in user code, but is inserted during elaboration.
        For example, if we have a [ x : Nat [= 7 ] ], we can convert [ x ] back
        to [ Nat ] by elaborating to [ SingElim (x, 7) ]. The [ 7 ] is used during
        typechecking (note the case in {!Core.Semantics.eval}), but should be
        erased in compiled code.
      *)
      | SingElim of tm * tm
    (** Telescopes *)
    and tele =
      | Nil
      | Cons of ty * (tele binds)

  end

  (** Semantic domain *)
  module Semantics = struct

    (** De-bruijn level *)
    type level = int

    (** Types *)
    type ty = tm

    (** Terms *)
    and tm =
      | Neu of neu
      | Univ
      | FunType of name * ty * (tm -> ty)
      | FunLit of name * (tm -> tm)
      | RecType of label list * tele
      | RecLit of (label * tm) list
      | SingType of ty * tm
      | SingIntro
    (** Telescopes *)
    and tele =
      | Nil
      | Cons of ty * (tm -> tele)
    (** Neutral terms *)
    and neu =
      | Var of level
      | FunApp of neu * tm
      | RecProj of neu * label

    (** Environment of bound entries that can be looked up directly using a
        {!Syntax.index}, or by inverting a {!level} using [ size - level - 1 ],
        where [ size ] is the number of entries bound in the environment. *)
    type 'a env = 'a list

    (** Internal error encountered in the semantics. These should never occur if
        terms are used in a way that respects the type system of the core
        language. *)
    exception Error of string

    (** Compute a function application *)
    let app : tm -> tm -> tm = function
      | FunLit (_, body) -> body
      | Neu neu -> fun arg -> Neu (FunApp (neu, arg))
      | _ -> raise (Error "invalid app")

    (** Compute a record projection *)
    let proj : tm -> label -> tm = function
      | RecLit fields -> fun label -> fields |> List.find (fun (l, _) -> l = label) |> snd
      | Neu neu -> fun label -> Neu (RecProj (neu, label))
      | _ -> raise (Error "invalid proj")

    (** Returns the type of a record projection *)
    let proj_ty head (labels, tele) label =
      let rec go labels tele =
        match labels, tele with
          | [], Nil -> None
          | l :: _, Cons (tm, _) when l = label -> Some tm
          | l :: ls, Cons (_, tms) -> go ls (tms (proj head l))
          | _, _ -> None in
      go labels tele

    (** Evaluate a term from the synatax into a term in the semantic domain *)
    let rec eval tms : Syntax.tm -> tm = function
      | Syntax.Let (_, def, body) -> eval (eval tms def :: tms) body
      | Syntax.Ann (tm, _) -> eval tms tm
      | Syntax.Var index -> List.nth tms index
      | Syntax.Univ -> Univ
      | Syntax.FunType (name, param_ty, body_ty) ->
          FunType (name, eval tms param_ty, fun x -> eval (x :: tms) body_ty)
      | Syntax.FunLit (name, body) -> FunLit (name, fun x -> eval (x :: tms) body)
      | Syntax.FunApp (head, arg) -> app (eval tms head) (eval tms arg)
      | Syntax.RecType (labels, tele) -> RecType (labels, eval_tele tms tele)
      | Syntax.RecLit fields ->
          RecLit (List.map (fun (label, expr) -> (label, eval tms expr)) fields)
      | Syntax.RecProj (head, label) -> proj (eval tms head) label
      | Syntax.SingType (ty, sing_tm) -> SingType (eval tms ty, eval tms sing_tm)
      | Syntax.SingIntro _ -> SingIntro
      | Syntax.SingElim (_, sing_tm) -> eval tms sing_tm
    and eval_tele tms : Syntax.tele -> tele = function
      | Syntax.Nil -> Nil
      | Syntax.Cons (tm, tele) ->
          Cons (eval tms tm, fun x -> eval_tele (x :: tms) tele)

    (** Typed quotation from the sematic domain back into the syntax.

        We only really use the types here as a way to restore the values of
        singletons that were erased during evaluation.
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
      | RecType (labels, tele) -> Syntax.RecType (labels, quote_tele size tys tele)
      | RecLit fields ->
          begin match ty with
          | RecType (_, tele) ->
              let rec go fields tele =
                match fields, tele with
                | [], Nil -> []
                | (label, tm) :: fields, Cons (ty, tele) ->
                    (label, quote size tys tm ty) :: go fields (tele tm)
                | _, _ -> raise (Error "mismatched telescope length")
              in
              Syntax.RecLit (go fields tele)
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
          | head', RecType (labels, tele) ->
              let ty = proj_ty (Neu head) (labels, tele) label |> Option.get in
              (Syntax.RecProj (head', label), ty)
          | _ -> raise (Error "not a record type")
          end
    and quote_tele size tys : tele -> Syntax.tele = function
      | Nil -> Syntax.Nil
      | Cons (ty, tele) ->
          let var = Neu (Var size) in
          Syntax.Cons (quote size tys ty Univ, quote_tele (size + 1) (ty :: tys) (tele var))

    (** Typed-directed conversion checking

        A type-directed approach allows us to support full eta for unit types.
        These show up in our language as empty records and singletons. If we
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
          | RecType (labels1, tele1), RecType (labels2, tele2) when labels1 = labels2 ->
              is_convertible_tele size tys tele1 tele2
          | SingType (ty1, sing_tm1), SingType (ty2, sing_tm2) ->
              is_convertible size tys ty1 ty2 Univ
                && is_convertible size tys sing_tm1 sing_tm2 ty1
          | _, _ -> false
          end
      | FunType (_, param_ty, body_ty) ->
          (* Eta for functions *)
          let var = Neu (Var size) in
          is_convertible (size + 1) (param_ty :: tys) (app tm1 var) (app tm2 var) (body_ty var)
      | RecType (labels, tele) ->
          (* Eta for records

            Record patching introduces subtyping problems that go inside records.
            With coercive subtyping (implemented in {!Surface.coerce}), record
            eta expansions are sometimes introduced in the core language that
            weren't present in the source syntax. This means that for usability
            it helps to include eta for records.
          *)
          let rec go labels tele =
            match labels, tele with
            | label :: labels, Cons (ty, tele) ->
                let tm1 = proj tm1 label in
                let tm2 = proj tm2 label in
                is_convertible size tys tm1 tm2 ty && go labels (tele tm1)
            | [], Nil -> true (* I think eta for units is hidden in here! *)
            | _, _ -> raise (Error "mismatched telescope length")
          in
          go labels tele
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
          | Some (RecType (labels, tele)) -> proj_ty (Neu record1) (labels, tele) label1
          | Some _ -> raise (Error "not a record type")
          | None -> None
          end
      | _, _ -> None
    and is_convertible_tele size tys tele1 tele2 =
      match tele1, tele2 with
      | Nil, Nil -> true
      | Cons (ty1, tele1), Cons (ty2, tele2) ->
          let var = Neu (Var size) in
          is_convertible size tys ty1 ty2 Univ
            && is_convertible_tele (size + 1) (ty1 :: tys) (tele1 var) (tele2 var)
      | _, _ -> false

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
        | RecType (labels, tele) ->
            str "{ " << go_field_tys size names labels tele << str "}"
        | RecLit fields ->
            str "{ " <<
              List.fold_right
                (fun (label, tm) rest ->
                  str label << str " := " << go false size names tm << str "; " << rest)
                fields (str "}")
        | SingType (ty, sing_tm) ->
            parens wrap (go false size names ty << str " [= " <<
              go false size names sing_tm << str " ]")
        | SingIntro -> str "sing-intro"
      and go_neu wrap size names = function
        | Var level -> str (List.nth names (size - level - 1))
        | FunApp (head, arg) ->
            parens wrap (go_neu false size names head << str " " <<  go true size names arg)
        | RecProj (head, label) -> go_neu false size names head << str "." << str label
      and go_field_tys size names labels tys =
        match labels, tys with
        | [], Nil -> str ""
        | label :: labels, Cons (ty, tys) ->
            str label << str " : " << go false size names ty << str "; " <<
              go_field_tys (size + 1) (label :: names) labels (tys (Neu (Var size)))
        | _, _ -> raise (Error "mismatched telescope length")
      in
      go false size names tm ""
    end

end

(** Surface language *)
module Surface = struct

  (** Terms in the surface language *)
  type tm =
    | Let of string * tm * tm             (** Let expressions: [ let x := t; f x ] *)
    | Name of string                      (** References to named things: [ x ] *)
    | Ann of tm * tm                      (** Terms annotated with types: [ x : A ] *)
    | Univ                                (** Universe of types: [ Type ] *)
    | FunType of (string * tm) list * tm  (** Function types: [ fun (x : A) -> B x ] *)
    | FunArrow of tm * tm                 (** Function arrow types: [ A -> B ] *)
    | FunLit of string list * tm          (** Function literals: [ fun x := f x ] *)
    | App of tm * tm                      (** Applications: [ f x ] *)
    | RecType of (string * tm) list       (** Record types: [ { x : A; ... } ]*)
    | RecLit of (string * tm) list        (** Record literals: [ { x := A; ... } ]*)
    | Proj of tm * string                 (** Projections: [ r.l ] *)
    | Patch of tm * (string * tm) list    (** Record type patching: [ R [ B := A; ... ] ] *)
    | SingType of tm * tm                 (** Singleton types: [ A [= x ] ] *)

  (** Note that we don’t need to add surface syntax for introducing and
      eliminating singletons, as these will be added implicitly during
      elaboration. For example, given [ a : A ] and [x : A [= a ] ]:

      - introduction: [ x : A [= a ] ] elaborates to [ sing-intro x ]
      - elimination: [ x : A ] elaborates to [ sing-elim x a ]
  *)

  module Syntax = Core.Syntax
  module Semantics = Core.Semantics

  (** Elaboration context *)
  type context = {
    size : Semantics.level;             (** Number of entries bound. *)
    names : Core.name Semantics.env;    (** Name environment *)
    tys : Semantics.ty Semantics.env;   (** Type environment *)
    tms : Semantics.tm Semantics.env;   (** Term environment *)
  }
  (** We could compute {!size} from the other environments, but because we are
      using linked lists for these this would be an [ O(n) ] operation. This
      allows us to access it in [ O(1) ].
  *)

  (** Empty elaboration context *)
  let initial_context = {
    size = 0;
    names = [];
    tys = [];
    tms = [];
  }

  (** Return the next variable that will be bound in the context after calling
      {!bind_def} or {!bind_param} *)
  let next_var context =
    Semantics.Neu (Semantics.Var context.size)

  (** Bind a definition in the context *)
  let bind_def context name ty tm = {
    size = context.size + 1;
    names = name :: context.names;
    tys = ty :: context.tys;
    tms = tm :: context.tms;
  }

  (** Bind a parameter in the context *)
  let bind_param context name ty =
    bind_def context name ty (next_var context)

  (** Elaboration error *)
  exception Error of string

  let eval context : Syntax.tm -> Semantics.tm =
    Semantics.eval context.tms
  let quote context : Semantics.tm -> Semantics.ty -> Syntax.tm =
    Semantics.quote context.size context.tys
  let is_convertible context : Semantics.tm -> Semantics.tm -> Semantics.ty -> bool =
    Semantics.is_convertible context.size context.tys
  let pretty context : Semantics.tm -> string =
    Semantics.pretty context.size context.names

  (** Coerce a term from one type to another type.

      By performing coercions during elaboration we avoid having to introduce
      subtyping in the core language.
  *)
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
    | Semantics.RecType (labels, from_tele), Semantics.RecType (labels', to_tele) when labels = labels' ->
        let rec go labels from_tele to_tele =
          match labels, from_tele, to_tele with
          | [], Semantics.Nil, Semantics.Nil -> []
          | label :: labels, Semantics.Cons (from_ty, from_tele), Semantics.Cons (to_ty, to_tele) ->
              let from_tm = eval context (Syntax.RecProj (tm, label)) in
              let to_tm = coerce context (Syntax.RecProj (tm, label)) from_ty to_ty in
              (label, to_tm) :: go labels (from_tele from_tm) (to_tele (eval context to_tm))
          | _, _, _ -> raise (Semantics.Error "mismatched telescope length")
        in
        Syntax.RecLit (go labels from_tele to_tele)
    (* TODO: subtyping for functions! *)
    | from_ty, to_ty  ->
        let expected = pretty context to_ty in
        let found = pretty context from_ty in
        raise (Error ("type mismatch: expected `" ^ expected ^ "`, found `" ^ found ^ "`"))

  (** Elaborate a term in the surface language into a term in the core language
      in the presence of a type annotation. *)
  let rec check context tm ty : Syntax.tm =
    match tm, ty with
    | Let (name, def, body), ty ->
        let def, def_ty = infer context def in
        let context = bind_def context name def_ty (eval context def) in
        Syntax.Let (name, def, check context body ty)
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
    | RecLit fields, Semantics.RecType (labels, tele) ->
        (* TODO: elaborate fields out of order? *)
        let rec go fields labels tele =
          match fields, labels, tele with
          | [], [], Semantics.Nil -> []
          | (label, tm) :: fields, label' :: labels, Semantics.Cons (ty, tele) when label = label' ->
              let tm = check context tm ty in
              (label, tm) :: go fields labels (tele (eval context tm))
          (* The definition of a missing field can be inferred from the record
             type if the field’s expected type is a singleton. This is a bit
             like in CoolTT: https://github.com/RedPRL/cooltt/pull/327 *)
          | fields, label :: labels, Semantics.Cons (Semantics.SingType (ty, sing_tm), tele) ->
              let tm = quote context sing_tm ty in
              (label, tm) :: go fields labels (tele (eval context tm))
          | _, label :: _, _ -> raise (Error ("field `" ^ label ^ "` is missing from record literal"))
          | (label, _) :: _, [], Semantics.Nil -> raise (Error ("unexpected field `" ^ label ^ "` in record literal"))
          | _, _, _ -> raise (Semantics.Error "mismatched telescope length")
        in
        Syntax.RecLit (go fields labels tele)
    | tm, Semantics.SingType (ty, sing_tm) ->
        let tm = check context tm ty in
        let tm' = eval context tm in
        if is_convertible context sing_tm tm' ty then Syntax.SingIntro tm else
          let expected = pretty context sing_tm in
          let found = pretty context tm' in
          let ty = pretty context ty in
          raise (Error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`"))
    | tm, ty ->
        let tm, ty' = infer_and_elim_implicits context tm in
        coerce context tm ty' ty

  (** Elaborate a term in the surface language into a term in the core language,
      inferring its type. *)
  and infer context : tm -> Syntax.tm * Semantics.ty = function
    | Let (name, def, body) ->
        let def, def_ty = infer context def in
        let def' = eval context def in
        let context = bind_def context name def_ty def' in
        let body, body_ty = infer context body in
        (Syntax.Let (name, def, body), body_ty)
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
    | App (head, arg) ->
        let head, head_ty = infer_and_elim_implicits context head in
        begin match head_ty with
        | Semantics.FunType (_, param_ty, body_ty) ->
            let arg = check context arg param_ty in
            (Syntax.FunApp (head, arg), body_ty (eval context arg))
        | _ -> raise (Error "not a function")
        end
    | RecType field_tys ->
        let rec go context seen_labels = function
          | [] -> ([], Syntax.Nil)
          | (label, _) :: _ when List.mem label seen_labels ->
              raise (Error ("duplicate label `" ^ label ^ "` in record type"))
          | (label, ty) :: field_tys ->
              let ty = check context ty Semantics.Univ in
              let context = bind_param context label (eval context ty) in
              let labels, tele = go context (label :: seen_labels) field_tys in
              (label :: labels, Syntax.Cons (ty, tele))
        in
        let labels, tele = go context [] field_tys in
        (Syntax.RecType (labels, tele), Semantics.Univ)
    | RecLit _ ->
        raise (Error "ambiguous record literal")
    | Proj (head, label) ->
        let head, head_ty = infer_and_elim_implicits context head in
        begin match head_ty with
        | Semantics.RecType (labels, tys) ->
            begin match Semantics.proj_ty (eval context head) (labels, tys) label with
            | Some ty -> (Syntax.RecProj (head, label), ty)
            | None -> raise (Error ("field `" ^ label ^ "` not found in record"))
            end
        | _ -> raise (Error "not a record")
        end
    | Patch (head, patches) ->
        let rec go context labels tys patches =
          match labels, tys, patches with
          | [], Semantics.Nil, [] -> Syntax.Nil
          | [], Semantics.Nil, (label, _) :: _ ->
              raise (Error ("field `" ^ label ^ "` not found in record type"))
          | label :: labels, Semantics.Cons (ty, tys), patches ->
              let ty' = quote context ty Univ in
              begin match List.assoc_opt label patches with
              | Some patch_tm ->
                  let tm = check context patch_tm ty in
                  let tm' = eval context tm in
                  let context = bind_def context label (Semantics.SingType (ty, tm')) tm' in
                  let patches = List.remove_assoc label patches in
                  Syntax.Cons (Syntax.SingType (ty', tm), go context labels (tys tm') patches)
              | None ->
                  let var = next_var context in
                  let context = bind_def context label ty var in
                  Syntax.Cons (ty', go context labels (tys var) patches)
              end
          | _, _, _ -> raise (Semantics.Error "mismatched telescope length")
        in
        let patch_labels = List.map fst patches in
        if List.sort_uniq (String.compare) patch_labels <> patch_labels then
          raise (Error "duplicate field labels in patches")
        else
          let head = check context head Semantics.Univ in
          begin match eval context head with
          | Semantics.RecType (labels, tys) ->
              let tys = go context labels tys patches in
              (Syntax.RecType (labels, tys), Semantics.Univ)
          | _ -> raise (Error "can only patch record types")
          end
    | SingType (ty, sing_tm) ->
        let ty = check context ty Semantics.Univ in
        let sing_tm = check context sing_tm (eval context ty) in
        (Syntax.SingType (ty, sing_tm), Semantics.Univ)

  (** Eliminate implicit connectives from a term.

      It's useful to employ this prior to calling {!coerce}, or when elaborating
      the head of an elimination form. *)
  and elim_implicits context tm = function
    (* Convert the singleton back to its underlying term using {!Syntax.SingElim} *)
    | Semantics.SingType (ty, sing_tm) ->
        let tm = Syntax.SingElim (tm, quote context sing_tm ty) in
        elim_implicits context tm ty
    (* TODO: we can eliminate implicit functions here. See the elaboration-zoo
      for ideas on how to do this: https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L48-L53 *)
    | ty -> (tm, ty)

  (** Elaborate a surface term, inferring its type and eliminating implicit
      connectives. *)
  and infer_and_elim_implicits context tm : Syntax.tm * Semantics.ty =
    let tm, ty = infer context tm in
    elim_implicits context tm ty

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

    let _ := (fun x := x) : F [ B := A ] -> F;
    let _ := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F;
    let _ := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A; f := fun x := x ] -> F;
    let _ := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ B := A ];
    let _ := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ A := A ];
    let _ := (fun C x := x) : fun (C : Type) -> F [ A := C; B := C ] -> F [ B := C ];

    let _ := (fun C := { f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ];
    let _ := (fun C := {}) : fun (C : Type) -> F [ A := C; B := C; f := fun x := x ];
    let _ := (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ];

    let _ :=
      (fun B r := r) :
        fun (B : Type) (r : { A : Type [= B ]; a : B })
          -> { A : Type; a : A };
    let _ :=
      (fun B b := { A := B; a := b } : { A : Type; a : B }) :
        fun (B : Type) (b : Type) -> { A : Type; a : A };

    let _ := (fun A x := x) : fun (A : Type) (x : A) -> A [= x ];
    let _ := (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [= x ]) -> A;

    let _ :=
      (fun A P f prf := prf) :
        fun (A : Type)
            (P : (fun (x : A) -> A [= x ]) -> Type)
            (f : fun (x : A) -> A [= x ])
            (prf : P (fun x := x))
        -> P f;

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

  (* let F := { A : Set; B : Set; f : A -> B }; *)
  let fun_record_ty =
    RecType [
      "A", Univ;
      "B", Univ;
      "f", FunArrow (Name "A", Name "B");
    ]

  let patch_tm1 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun x := x) : F [ B := A ] -> F *)
      Ann (FunLit (["x"], Name "x"),
        FunArrow (Patch (Name "F", ["B", Name "A"]), Name "F")))

  let patch_tm2 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]), Name "F"))))

  let patch_tm3 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A; f := fun x := x ] -> F *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"; "f", FunLit (["x"], Name "x")]),
            Name "F"))))

  let patch_tm4 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ B := A ] *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
            Patch (Name "F", ["B", Name "A"])))))

  let patch_tm5 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ A := A ] *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
            Patch (Name "F", ["A", Name "A"])))))

  let patch_tm6 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C x := x) : fun (C : Type) -> F [ A := C; B := C ] -> F [ B := C ] *)
      Ann (FunLit (["C"; "x"], Name "x"),
        FunType (["C", Univ],
          FunArrow (Patch (Name "F", ["A", Name "C"; "B", Name "C"]),
            Patch (Name "F", ["B", Name "C"])))))

  let patch_tm_lit1 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C := { f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ] *)
      Ann (FunLit (["C"], RecLit ["f", FunLit (["x"], Name "x")]),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  let patch_tm_lit2 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C := {}) : fun (C : Type) -> F [ A := C; B := C; f := fun x := x ] *)
      Ann (FunLit (["C"], RecLit []),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"; "f", FunLit (["x"], Name "x")]))))

  let patch_tm_lit3 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
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
        "prf", App (Name "P", FunLit (["x"], Name "x"));
      ], App (Name "P", Name "f")))

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
      "seq", FunType (["f", Name "Hom"; "g", Patch (Name "Hom", ["s", Proj (Name "f", "t")])],
        Patch (Name "Hom", ["s", Proj (Name "f", "s"); "t", Proj (Name "g", "t")]));
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
    Let ("category", category_ty,
      Ann (
        RecLit [
          "Ob", Univ;
          "Hom", FunLit (["params"], FunArrow (Proj (Name "params", "s"), Proj (Name "params", "t")));
          "id", FunLit (["A"; "a"], Name "a");
          "seq", FunLit (["f"; "g"; "a"], App (Name "g", App (Name "f", Name "a")));
        ],
        Name "category"))

  let terms = [
    "fun_record_ty", fun_record_ty;
    "patch_tm1", patch_tm1;
    "patch_tm2", patch_tm2;
    "patch_tm3", patch_tm3;
    "patch_tm4", patch_tm4;
    "patch_tm5", patch_tm5;
    "patch_tm6", patch_tm6;
    "patch_tm_lit1", patch_tm_lit1;
    "patch_tm_lit2", patch_tm_lit2;
    "patch_tm_lit3", patch_tm_lit3;
    "record_lit_coerce1", record_lit_coerce1;
    "record_lit_coerce2", record_lit_coerce2;
    "intro_sing", intro_sing;
    "elim_sing", elim_sing;
    "sing_tm1", sing_tm1;
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

(* Syntax bikeshed

  Record patching:

  -  [ R [ B := A; ... ] ]
  -  [ R.{ B := A; ... } ]
  -  [ R # [ B .= A, ... ] ] (like in CoolTT)
  -  [ R # { B := A; ... } ]
  -  [ R (B := A, ...) ] (possibly overloaded with function application)
  -  [ R where B := A, ... ] (like in Standard-ML)

  Singleton types:

  - [ A [ x ] ]
  - [ A [= x ] ] (riffing on the idea of using square brackets for ‘refinement’)
  - [ A [:= x ] ] (another similar idea)
  - [ (= x : A) ]
  - [ (:= x : A) ]
  - [ A (= x) ]
  - [ A (:= x) ] (parens read like, “btw, it's equal to [ x ]”)
*)
