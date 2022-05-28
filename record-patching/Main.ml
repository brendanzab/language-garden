(** {0 Elaboration with Singletons and Record patching}

    Record patching is a way to constrain the values of fields in a record type.
    Given a record type [ R ], a record patch can be applied using the syntax
    [ R.{ l := t; ... } ]. For example:

    {[
      let Monoid := {
        T : Type;
        empty : T;
        append : T -> T -> T;
      };

      let string-monoid : Monoid.{ T := String } := {
        T := String;
        empty := "";
        append := string-append;
      };
    ]}

    This is similar to Standard ML’s [ where type ] {{: https://smlfamily.github.io/sml97-defn.pdf#page=28}
    syntax for type realisation}, OCaml’s [ with ] {{: https://v2.ocaml.org/manual/modtypes.html#ss%3Amty-with}
    operator for constraining module types}, or Rust’s [ Iterator<Item = T> ]
    shorthand syntax for {{: https://rust-lang.github.io/rfcs/0195-associated-items.html#constraining-associated-types}
    equality constraints} in type parameter bounds.

    Patches are soley a feature of the surface language and are removed during
    elaboration. The expression [ Monoid.{ T := String } ] in the example above
    elaborates to a new record type, where the type of the [ T ] field is
    constrained to [ String ] through the use of a singleton type:

    {[
      {
        T : Type [ String ];
        empty : T;
        append : T -> T -> T;
      }
    ]}

    We can also infer the definitions of fields from singletons. This works
    nicely in combination with record patching. Note how we don't need to
    mention the [ T ] field in the following record literal:

    {[
      let nat-add-monoid : Monoid.{ T := Nat } := {
        empty := 0;
        append := fun x y := x + y;
      };

      let nat-mul-monoid : Monoid.{ T := Nat } := {
        empty := 1;
        append := fun x y := x * y;
      };
    ]}

    Patches don’t need to be limited to types either:

    {[
      let nat-mul-monoid-2 : Monoid.{ T := Nat; empty := 1 } :=
        nat-mul-monoid;
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
*)

type name = string
type label = string

type index = int
type level = int

(** Returns the index of the given element in the list *)
let elem_index a =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = a then Some i else go (i + 1) xs in
  go 0

(** Core language *)
module Core = struct
  (** Core Syntax *)
  module Syntax = struct
    (** For documenting where variables are bound *)
    type 'a binds = 'a

    type ty = tm
    and tm =
      | Let of name * tm * tm
      | Var of index
      | Univ
      | Pi of name * ty * (ty binds)
      | Lam of name * (tm binds)
      | App of tm * tm
      | Sig of label list * tele
      | Record of (label * tm) list
      | Proj of tm * label
      (* Singleton type former, eg. [ A [ x ] ].

        These types constrain a type [ A ] to be equal to a single value [ x ].
      *)
      | Sing of ty * tm
      (* Introduction form for singletons.

        This does not appear in user code, but is inserted during elaboration.
        For example, if [ x : Nat ] and [ x ≡ 7 ], using [ x ] as [ Nat [ 7 ] ]
        elaborates to [ MkSing x ].
      *)
      | MkSing of tm
      (* Elimination form for singletons.

        This does not appear in user code, but is inserted during elaboration.
        For example, if we have a [ x : Nat [ 7 ] ], we can convert [ x ] back
        to [ Nat ] by elaborating to [ UnSing (x, 7) ]. The [ 7 ] is used during
        typechecking (note the case in {!Core.Semantics.eval}), but should be
        erased in compiled code.
      *)
      | UnSing of tm * tm
    and tele =
      | Nil
      | Cons of ty * (tele binds)
  end

  (** Semantic domain *)
  module Semantics = struct
    type ty = tm
    and tm =
      | Neu of neu
      | Univ
      | Pi of name * ty * (tm -> ty)
      | Lam of name * (tm -> tm)
      | Sig of label list * tele
      | Record of (label * tm) list
      | Sing of ty * tm
      | MkSing
    and tele =
      | Nil
      | Cons of ty * (tm -> tele)
    and neu =
      | Var of level
      | App of neu * tm
      | Proj of neu * label

    exception Error of string

    let app : tm -> tm -> tm = function
      | Lam (_, body) -> body
      | Neu neu -> fun arg -> Neu (App (neu, arg))
      | _ -> raise (Error "invalid app")

    let proj : tm -> label -> tm = function
      | Record fields -> fun label ->
          fields |> List.find (fun (l, _) -> l = label) |> snd
      | Neu neu -> fun label -> Neu (Proj (neu, label))
      | _ -> raise (Error "invalid proj")

    let proj_ty head (labels, tele) label =
      let rec go labels tele =
        match labels, tele with
          | [], Nil -> None
          | l :: _, Cons (tm, _) when l = label -> Some tm
          | l :: ls, Cons (_, tms) -> go ls (tms (proj head l))
          | _, _ -> None in
      go labels tele

    let rec eval env : Syntax.tm -> tm = function
      | Syntax.Let (_, def, body) -> eval (eval env def :: env) body
      | Syntax.Var index -> List.nth env index
      | Syntax.Univ -> Univ
      | Syntax.Pi (name, param_ty, body_ty) -> Pi (name, eval env param_ty, fun x -> eval (x :: env) body_ty)
      | Syntax.Lam (name, body) -> Lam (name, fun x -> eval (x :: env) body)
      | Syntax.App (head, arg) -> app (eval env head) (eval env arg)
      | Syntax.Sig (labels, tys) -> Sig (labels, eval_tele env tys)
      | Syntax.Record fields -> Record (List.map (fun (label, expr) -> (label, eval env expr)) fields)
      | Syntax.Proj (head, label) -> proj (eval env head) label
      | Syntax.Sing (ty, sing_tm) -> Sing (eval env ty, eval env sing_tm)
      | Syntax.MkSing _ -> MkSing
      | Syntax.UnSing (_, sing_tm) -> eval env sing_tm
    and eval_tele env : Syntax.tele -> tele = function
      | Syntax.Nil -> Nil
      | Syntax.Cons (tm, tms) ->
          Cons (eval env tm, fun x -> eval_tele (x :: env) tms)

    (** Typed quotation from the sematic domain back into the syntax. *)
    let rec quote size tys tm ty : Syntax.tm =
      match tm with
      | Neu neu -> fst (quote_neu size tys neu)
      | Univ -> Syntax.Univ
      | Pi (name, param_ty, body_ty) ->
          let x = Neu (Var size) in
          let param_ty = quote size tys param_ty Univ in
          let body_ty = quote (size + 1) (Univ :: tys) (body_ty x) Univ in
          Syntax.Pi (name, param_ty, body_ty)
      | Lam (name, body) ->
          begin match ty with
          | Pi (_, param_ty, body_ty) ->
              let x = Neu (Var size) in
              Syntax.Lam (name, quote (size + 1) (param_ty :: tys) (body x) (body_ty x))
          | _ -> raise (Error "not a function type")
          end
      | Sig (labels, tele) -> Syntax.Sig (labels, quote_tele size tys tele)
      | Record fields ->
          begin match ty with
          | Sig (_, tele) ->
              let rec go fields tele =
                match fields, tele with
                | [], Nil -> []
                | (label, tm) :: fields, Cons (ty, tele) ->
                    (label, quote size tys tm ty) :: go fields (tele tm)
                | _, _ -> raise (Error "mismatched telescope length")
              in
              Syntax.Record (go fields tele)
          | _ -> raise (Error "not a record type")
          end
      | Sing (ty, sing_tm) ->
          Syntax.Sing (quote size tys ty Univ, quote size tys sing_tm ty)
      | MkSing ->
          begin match ty with
          (* Restore the erased term from the singleton type *)
          | Sing (ty, sing_tm) -> Syntax.MkSing (quote size tys ty sing_tm)
          | _ -> raise (Error "not a singleton type")
          end
    and quote_neu size tys : neu -> Syntax.tm * ty = function
      | Var level ->
          let index = size - level - 1 in
          (Syntax.Var index, List.nth tys index)
      | App (head, arg) ->
          begin match quote_neu size tys head with
          | head, Pi (_, param_ty, body_ty) ->
              (Syntax.App (head, (quote size tys arg param_ty)), body_ty arg)
          | _ -> raise (Error "not a function type")
          end
      | Proj (head, label) ->
          begin match quote_neu size tys head with
          | head', Sig (labels, tele) ->
              let ty = proj_ty (Neu head) (labels, tele) label |> Option.get in
              (Syntax.Proj (head', label), ty)
          | _ -> raise (Error "not a record type")
          end
    and quote_tele size tys : tele -> Syntax.tele = function
      | Nil -> Syntax.Nil
      | Cons (ty, tele) ->
          let x = Neu (Var size) in
          Syntax.Cons (quote size tys ty Univ, quote_tele (size + 1) (ty :: tys) (tele x))

    (** Typed conversion checking *)
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
          | Pi (_, param_ty1, body_ty1), Pi (_, param_ty2, body_ty2) ->
              let x = Neu (Var size) in
              is_convertible size tys param_ty1 param_ty2 Univ
                && is_convertible (size + 1) (param_ty1 :: tys) (body_ty1 x) (body_ty2 x) Univ
          | Sig (labels1, tele1), Sig (labels2, tele2) when labels1 = labels2 ->
              is_convertible_tele size tys tele1 tele2
          | Sing (ty1, sing_tm1), Sing (ty2, sing_tm2) ->
              is_convertible size tys ty1 ty2 Univ
                && is_convertible size tys sing_tm1 sing_tm2 ty1
          | _, _ -> false
          end
      | Pi (_, param_ty, body_ty) ->
          (* Eta for functions *)
          let x = Neu (Var size) in
          is_convertible (size + 1) (param_ty :: tys) (app tm1 x) (app tm2 x) (body_ty x)
      | Sig (labels, tele) ->
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
      | Sing (_, _) -> true
      | _ -> raise (Error "not a type")
    and is_convertible_neu size tys neu1 neu2 =
      match neu1, neu2 with
      | Var level1, Var level2 when level1 = level2 -> Some (List.nth tys (size - level1 - 1))
      | App (func1, arg1), App (func2, arg2) ->
          begin match is_convertible_neu size tys func1 func2 with
          | Some (Pi (_, param_ty, body_ty)) ->
              if is_convertible size tys arg1 arg2 param_ty then Some (body_ty arg1) else None
          | Some _ -> raise (Error "not a function type")
          | None -> None
          end
      | Proj (record1, label1), Proj (record2, label2) when label1 = label2 ->
          begin match is_convertible_neu size tys record1 record2 with
          | Some (Sig (labels, tele)) -> proj_ty (Neu record1) (labels, tele) label1
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

    let pretty size names tm =
      let (<<) f g x = f (g x) in
      let str s rest = s ^ rest in
      let parens wrap s rest = if wrap then "(" ^ s (")" ^ rest) else s rest in
      let rec go wrap size names = function
        | Neu neu -> parens wrap (go_neu size names neu)
        | Univ -> str "Type"
        | Pi (name, param_ty, body_ty) ->
            parens wrap (str "fun (" << str name << str " : " << go false size names param_ty <<
              str ") -> " << go false (size + 1) (name :: names) (body_ty (Neu (Var size))))
        | Lam (name, body) ->
            parens wrap (str "fun " << str name << str " := " <<
              go false (size + 1) (name :: names) (body (Neu (Var size))))
        | Sig (labels, tys) ->
            str "{ " << go_field_tys size names labels tys << str "}"
        | Record fields ->
            str "{ " <<
              List.fold_right
                (fun (label, tm) rest ->
                  str label << str " := " << go false size names tm << str "; " << rest)
                fields (str "}")
        | Sing (ty, sing_tm) ->
            parens wrap (go false size names ty << str " [ " <<
              go false size names sing_tm << str " ]")
        | MkSing -> str "mk-sing"
      and go_neu size names = function
        | Var level -> str (List.nth names (size - level - 1))
        | App (head, arg) -> go_neu size names head << str " " << go true size names arg
        | Proj (head, label) -> go_neu size names head << str "." << str label
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
  module Syntax = Core.Syntax
  module Semantics = Core.Semantics

  type tm =
    | Let of name * tm * tm
    | Name of name
    | Ann of tm * tm
    | Univ
    | Pi of (name * tm) list * tm
    | Arrow of tm * tm
    | Lam of name list * tm
    | App of tm * tm
    | Sig of (label * tm) list
    | Record of (label * tm) list
    | Proj of tm * label
    (* Record type patching, eg. [ R.{ B := A; ... } ]

      Syntax bikeshed:

      -  [ R [ B := A; ... ] ]
      -  [ R.{ B := A; ... } ]
      -  [ R # [ B .= A, ... ] ] (like in CoolTT)
      -  [ R # { B := A; ... } ]
      -  [ R (B := A, ...) ] (possibly overloaded with function application)
      -  [ R where B := A, ... ] (like in Standard-ML)
    *)
    | Patch of tm * (label * tm) list
    (* Singleton types, eg. [ A [ x ] ]

      There’s no need to add surface syntax for introducing and eliminating
      singletons, as this is done implicitly during elaboration:

      - introduction: [ x : A [ x ] ]
      - elimination: [ (x : A [ x ]) : A ]

      Syntax bikeshed:

      - [ A [ x ] ]
      - [ A [= x ] ] (riffing on the idea of using square brackets for ‘refinement’)
      - [ A [:= x ] ] (another similar idea)
      - [ (= x : A) ]
      - [ (:= x : A) ]
      - [ A (= x) ]
      - [ A (:= x) ] (parens read like, “btw, it's equal to [ x ]”)
    *)
    | Sing of tm * tm

  (** Elaboration context *)
  type context = {
    (* Number of entries bound in the context. We could compute this from the
       other environments, but given we are using linked lists this would be an
       [ O(n) ] operation. *)
    size : level;
    names : name list;
    tys : Semantics.ty list;
    tms : Semantics.tm list;
  }

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
    (* Coerce the term to a singleton with {!Syntax.MkSing}, if the term is
      convertible to the term expected by the singleton *)
    | from_ty, Semantics.Sing (to_ty, sing_tm) ->
        let tm = coerce context tm from_ty to_ty in
        let tm' = eval context tm in
        if is_convertible context sing_tm tm' to_ty then Syntax.MkSing tm else
          let expected = pretty context sing_tm in
          let found = pretty context tm' in
          let ty = pretty context to_ty in
          raise (Error ("mismatched singleton: expected `" ^ expected ^ "`, found `" ^ found ^ "` of type `" ^ ty ^ "`"))
    (* Coerce the singleton back to its underlying term with {!Syntax.UnSing}
      and attempt further coercions from its underlying type *)
    | Semantics.Sing (from_ty, sing_tm), to_ty ->
        let tm = Syntax.UnSing (tm, quote context sing_tm from_ty) in
        coerce context tm from_ty to_ty
    (* Coerce the fields of a record with record eta expansion *)
    | Semantics.Sig (labels, from_tys), Semantics.Sig (labels', to_tys) when labels = labels' ->
        let rec go labels from_tys to_tys =
          match labels, from_tys, to_tys with
          | [], Semantics.Nil, Semantics.Nil -> []
          | label :: labels, Semantics.Cons (from_ty, from_tys), Semantics.Cons (to_ty, to_tys) ->
              let from_tm = eval context (Syntax.Proj (tm, label)) in
              let to_tm = coerce context (Syntax.Proj (tm, label)) from_ty to_ty in
              (label, to_tm) :: go labels (from_tys from_tm) (to_tys (eval context to_tm))
          | _, _, _ -> raise (Semantics.Error "mismatched telescope length")
        in
        Syntax.Record (go labels from_tys to_tys)
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
    | Lam (names, body), ty ->
        let rec go context names ty =
          match names, ty with
          | [], body_ty -> check context body body_ty
          | name :: names, Semantics.Pi (_, param_ty, body_ty) ->
              let var = next_var context in
              let context = bind_def context name param_ty var in
              Syntax.Lam (name, go context names (body_ty var))
          | _, _ -> raise (Error "too many parameters in function literal")
        in
        go context names ty
    | Record fields, Semantics.Sig (labels, tys) ->
        (* TODO: elaborate fields out of order? *)
        let rec go fields labels tys =
          match fields, labels, tys with
          | [], [], Semantics.Nil -> []
          | (label, tm) :: fields, label' :: labels, Semantics.Cons (ty, tys) when label = label' ->
              let tm = check context tm ty in
              (label, tm) :: go fields labels (tys (eval context tm))
          (* If the field is missing from the record literal we can infer it if
            the expected field was fixed with a singleton type. This is a bit
            like in CoolTT: https://github.com/RedPRL/cooltt/pull/327 *)
          | fields, label :: labels, Semantics.Cons (Semantics.Sing (ty, sing_tm), tys) ->
              let tm = quote context sing_tm ty in
              (label, tm) :: go fields labels (tys (eval context tm))
          | _, label :: _, _ -> raise (Error ("field `" ^ label ^ "` is missing from record literal"))
          | (label, _) :: _, [], Semantics.Nil -> raise (Error ("unexpected field `" ^ label ^ "` in record literal"))
          | _, _, _ -> raise (Semantics.Error "mismatched telescope length")
        in
        Syntax.Record (go fields labels tys)
    | tm, Semantics.Sing (ty, sing_tm) ->
        let tm = check context tm ty in
        let tm' = eval context tm in
        if is_convertible context sing_tm tm' ty then Syntax.MkSing tm else
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
        (tm, ty')
    | Univ ->
        (Syntax.Univ, Semantics.Univ)
    | Pi (params, body_ty) ->
        let rec go context = function
          | [] -> check context body_ty Semantics.Univ
          | (name, param_ty) :: params ->
              let param_ty = check context param_ty Semantics.Univ in
              let context = bind_param context name (eval context param_ty) in
              Syntax.Pi (name, param_ty, go context params)
        in
        (go context params, Semantics.Univ)
    | Arrow (param_ty, body_ty) ->
        let param_ty = check context param_ty Semantics.Univ in
        let context = bind_param context "_" (eval context param_ty) in
        let body_ty = check context body_ty Semantics.Univ in
        (Syntax.Pi ("_", param_ty, body_ty), Semantics.Univ)
    | Lam (_, _) ->
        raise (Error "ambiguous function literal")
    | App (head, arg) ->
        let head, head_ty = infer_and_elim_implicits context head in
        begin match head_ty with
        | Semantics.Pi (_, param_ty, body_ty) ->
            let arg = check context arg param_ty in
            (Syntax.App (head, arg), body_ty (eval context arg))
        | _ -> raise (Error "not a function")
        end
    | Sig field_tys ->
        let rec go context seen_labels = function
          | [] -> ([], Syntax.Nil)
          | (label, _) :: _ when List.mem label seen_labels ->
              raise (Error ("duplicate label `" ^ label ^ "` in record type"))
          | (label, ty) :: field_tys ->
              let ty = check context ty Semantics.Univ in
              let context = bind_param context label (eval context ty) in
              let labels, tys = go context (label :: seen_labels) field_tys in
              (label :: labels, Syntax.Cons (ty, tys))
        in
        let labels, tys = go context [] field_tys in
        (Syntax.Sig (labels, tys), Semantics.Univ)
    | Record _ ->
        raise (Error "ambiguous record literal")
    | Proj (head, label) ->
        let head, head_ty = infer_and_elim_implicits context head in
        begin match head_ty with
        | Semantics.Sig (labels, tys) ->
            begin match Semantics.proj_ty (eval context head) (labels, tys) label with
            | Some ty -> (Syntax.Proj (head, label), ty)
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
                  let context = bind_def context label (Semantics.Sing (ty, tm')) tm' in
                  let patches = List.remove_assoc label patches in
                  Syntax.Cons (Syntax.Sing (ty', tm), go context labels (tys tm') patches)
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
          | Semantics.Sig (labels, tys) ->
              let tys = go context labels tys patches in
              (Syntax.Sig (labels, tys), Semantics.Univ)
          | _ -> raise (Error "can only patch record types")
          end
    | Sing (ty, sing_tm) ->
        let ty = check context ty Semantics.Univ in
        let sing_tm = check context sing_tm (eval context ty) in
        (Syntax.Sing (ty, sing_tm), Semantics.Univ)

  (** Eliminate implicit connectives from a term.

      It's useful to employ this prior to calling {!coerce}, or when elaborating
      the head of an elimination form.
  *)
  and elim_implicits context tm = function
    (* Convert the singleton back to its underlying term using {!Syntax.UnSing} *)
    | Semantics.Sing (ty, sing_tm) ->
        let tm = Syntax.UnSing (tm, quote context sing_tm ty) in
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

(** Example terms, for testing purposes *)
(* TODO: Implement a parser and convert to promote tests *)
module Examples = struct
  open Surface

  (*
    let F := {
      A : Type;
      B : Type;
      f : A -> B;
    };

    let _ := (fun x := x) : F.{ B := A } -> F;
    let _ := (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A } -> F;
    let _ := (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A; f := fun x := x } -> F;
    let _ := (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A } -> F.{ B := A };
    let _ := (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A } -> F.{ A := A };
    let _ := (fun C x := x) : fun (C : Type) -> F.{ A := C; B := C } -> F.{ B := C };

    let _ := (fun C := { f := fun x := x }) : fun (C : Type) -> F.{ A := C; B := C };
    let _ := (fun C := {}) : fun (C : Type) -> F.{ A := C; B := C; f := fun x := x };
    let _ := (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F.{ A := C; B := C };

    let _ :=
      (fun B r := r) :
        fun (B : Type) (r : { A : Type [ B ]; a : B })
          -> { A : Type; a : A };
    let _ :=
      (fun B b := { A := B; a := b } : { A : Type; a : B }) :
        fun (B : Type) (b : Type) -> { A : Type; a : A };

    let _ := (fun A x := x) : fun (A : Type) (x : A) -> A [ x ];
    let _ := (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [ x ]) -> A;

    let _ :=
      (fun A P f prf := prf) :
        fun (A : Type)
            (P : (fun (x : A) -> A [ x ]) -> Type)
            (f : fun (x : A) -> A [ x ])
            (prf : P (fun x := x))
        -> P f;

    -- TODO: requires total space conversion like in CoolTT

    let category := {
      Ob : Type;
      Hom : { s : Ob; t : Ob } -> Type;
      id : fun (x : Ob) -> Hom.{ s := x; t := x };
      seq : fun (f : Hom) (g : Hom.{ s := f.t }) -> Hom.{ s := f.s; t := g.t };
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
    Sig [
      "A", Univ;
      "B", Univ;
      "f", Arrow (Name "A", Name "B");
    ]

  let patch_tm1 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun x := x) : F.{ B := A } -> F *)
      Ann (Lam (["x"], Name "x"),
        Arrow (Patch (Name "F", ["B", Name "A"]), Name "F")))

  let patch_tm2 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A } -> F *)
      Ann (Lam (["A"; "x"], Name "x"),
        Pi (["A", Univ],
          Arrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]), Name "F"))))

  let patch_tm3 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A; f := fun x := x } -> F *)
      Ann (Lam (["A"; "x"], Name "x"),
        Pi (["A", Univ],
          Arrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"; "f", Lam (["x"], Name "x")]),
            Name "F"))))

  let patch_tm4 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A } -> F.{ B := A } *)
      Ann (Lam (["A"; "x"], Name "x"),
        Pi (["A", Univ],
          Arrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
          Patch (Name "F", ["B", Name "A"])))))

  let patch_tm5 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F.{ A := A; B := A } -> F.{ A := A } *)
      Ann (Lam (["A"; "x"], Name "x"),
        Pi (["A", Univ],
          Arrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
          Patch (Name "F", ["A", Name "A"])))))

  let patch_tm6 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C x := x) : fun (C : Type) -> F.{ A := C; B := C } -> F.{ B := C } *)
      Ann (Lam (["C"; "x"], Name "x"),
        Pi (["C", Univ],
          Arrow (Patch (Name "F", ["A", Name "C"; "B", Name "C"]),
          Patch (Name "F", ["B", Name "C"])))))

  let patch_tm_lit1 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C := { f := fun x := x }) : fun (C : Type) -> F.{ A := C; B := C } *)
      Ann (Lam (["C"], Record ["f", Lam (["x"], Name "x")]),
        Pi (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  let patch_tm_lit2 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C := {}) : fun (C : Type) -> F.{ A := C; B := C; f := fun x := x } *)
      Ann (Lam (["C"], Record []),
        Pi (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"; "f", Lam (["x"], Name "x")]))))

  let patch_tm_lit3 =
    (* let F := { A : Set; B : Set; f : A -> B }; *)
    Let ("F", fun_record_ty,
      (* (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F.{ A := C; B := C } *)
      Ann (Lam (["C"], Record ["B", Name "C"; "f", Lam (["x"], Name "x")]),
        Pi (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  (*
    (fun B r := r) :
      fun (B : Type) (r : { A : Type [ B ]; a : B })
        -> { A : Type; a : A }
  *)
  let record_lit_coerce1 =
    Ann (Lam (["B"; "r"], Name "r"),
      Pi (["B", Univ; "r", Sig ["A", Sing (Univ, Name "B"); "a", Name "B"]],
        Sig ["A", Univ; "a", Name "A"]))

  (*
    (fun B b := { A := B; a := b } : { A : Type; a : B }) :
      fun (B : Type) (b : Type) -> { A : Type; a : A }
  *)
  let record_lit_coerce2 =
    Ann (Lam (["B"; "b"],
        Ann (Record ["A", Name "B"; "a", Name "b"],
          Sig ["A", Univ; "a", Name "B"])),
      Pi (["B", Univ; "b", Name "B"], Sig ["A", Univ; "a", Name "A"]))

  (*
     (fun A x := x) : fun (A : Type) (x : A) -> A [ x ]
  *)
  let intro_sing =
    Ann (Lam (["A"; "x"], Name "x"),
      Pi (["A", Univ; "x", Name "A"], Sing (Name "A", Name "x")))

  (*
     (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [ x ]) -> A
  *)
  let elim_sing =
    Ann (Lam (["A"; "x"; "sing-x"], Name "sing-x"),
      Pi (["A", Univ; "x", Name "A"; "sing-x", Sing (Name "A", Name "x")], Name "A"))

  (*
    (fun A P f pf := pf) :
      fun (A : Type)
          (P : (fun (x : A) -> A [ x ]) -> Type)
          (f : fun (x : A) -> A [ x ])
          (pf : P (fun x := x))
      -> P f
  *)
  let sing_tm1 =
    Ann (Lam (["A"; "P"; "f"; "prf"], Name "prf"),
      Pi ([
        "A", Univ;
        "P", Pi (["_", Pi (["x", Name "A"], Sing (Name "A", Name "x"))], Univ);
        "f", Pi (["x", Name "A"], Sing (Name "A", Name "x"));
        "prf", App (Name "P", Lam (["x"], Name "x"));
      ], App (Name "P", Name "f")))

  (*
    let category := {
      Ob : Type;
      Hom : { s : Ob; t : Ob } -> Type;
      id : fun (x : Ob) -> Hom.{ s := x; t := x };
      seq : fun (f : Hom) (g : Hom.{ s := f.t }) -> Hom.{ s := f.s; t := g.t };
    };
  *)
  let category_ty =
    Sig [
      "Ob", Univ;
      "Hom", Arrow (Sig ["s", Name "Ob"; "t", Name "Ob"], Univ);
      "id", Pi (["x", Name "Ob"], Patch (Name "Hom", ["s", Name "x"; "t", Name "x"]));
      "seq", Pi (["f", Name "Hom"; "g", Patch (Name "Hom", ["s", Proj (Name "f", "t")])],
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
        Record [
          "Ob", Univ;
          "Hom", Lam (["params"], Arrow (Proj (Name "params", "s"), Proj (Name "params", "t")));
          "id", Lam (["A"; "a"], Name "a");
          "seq", Lam (["f"; "g"; "a"], App (Name "g", App (Name "f", Name "a")));
        ],
        category_ty
      ))

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
    (* TODO: these require total space conversion, like in CoolTT *)
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
