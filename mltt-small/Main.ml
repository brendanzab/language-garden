(** {0 An implementation of a small dependently typed language}

    This is an implementation simple dependently typed language where types are
    first-class and where the output types of functions may depend on the inputs
    supplied to them.

    Type checking is is implemented in terms of an {i elaborator}, which checks
    and tanslates a user-friendly {i surface language} into a simpler and more
    explicit {i core language} that is more closely connected to type theory.
    Because we need to simplify types during elaboration we also implement an
    interpreter for the core language.
*)

(** Returns the index of the given element in the list *)
let elem_index a =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = a then Some i else go (i + 1) xs in
  go 0

(** Core language *)
module Core = struct
  (** The core language is intended to be minimal, and close to well-understood
      type theories. The majority of this module is split up into the {!Syntax}
      and the {!Semantics}. *)


  (** {1 Names} *)

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

    (** Types *)
    type ty = tm

    (** Terms *)
    and tm =
      | Let of name * tm * tm
      | Ann of tm * ty
      | Var of index
      | Univ
      | FunType of name * ty * ty
      | FunLit of name * tm
      | FunApp of tm * tm


    (** {1 Pretty printing} *)

    let pretty names tm =
      let concat = String.concat "" in
      let parens wrap s = if wrap then concat ["("; s; ")"] else s in
      let rec go wrap names = function
        | Let (name, def, body) ->
            parens wrap (concat ["let "; name; " := "; go false names def; "; ";
              go false (name :: names) body])
        | Ann (tm, ty) -> parens wrap (concat [go true names tm; " : "; go false names ty])
        | Var index -> List.nth names index
        | Univ -> "Type"
        | FunType (name, param_ty, body_ty) ->
            parens wrap (concat ["fun ("; name; " : "; go false names param_ty;
              ") -> "; go false (name :: names) body_ty])
        | FunLit (name, body) ->
            parens wrap (concat ["fun "; name; " := ";
              go false (name :: names) body])
        | FunApp (head, arg) ->
            parens wrap (concat [go false names head; " ";
              go true names arg])
      in
      go false names tm

  end

  (** Semantics of the core language *)
  module Semantics = struct

    (** {1 Semantic domain} *)

    (** Types *)
    type ty = tm

    (** Terms in weak head normal form *)
    and tm =
      | Neu of neu                            (** Neutral terms *)
      | Univ
      | FunType of string * ty Lazy.t * (tm -> ty)
      | FunLit of string * (tm -> tm)

    (** Neutral terms are terms that could not be reduced to a normal form as a
        result of being stuck on something else that would not reduce further.
        I’m not sure why they are called ‘neutral terms’. Perhaps they are...
        ambivalent about what they might compute to? *)
    and neu =
      | Var of level                (** Variable that could not be reduced further *)
      | FunApp of neu * tm Lazy.t   (** Function application *)

    (** An environment of bindings that can be looked up directly using a
        {!Syntax.index}, or by inverting a {!level} using {!level_to_index}. *)
    type 'a env = 'a list


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
      | Neu neu -> Neu (FunApp (neu, lazy arg))
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
          FunType (name, lazy (eval env param_ty), fun x -> eval (x :: env) body_ty)
      | Syntax.FunLit (name, body) -> FunLit (name, fun x -> eval (x :: env) body)
      | Syntax.FunApp (head, arg) -> app (eval env head) (eval env arg)


    (** {1 Quotation} *)

    (** Quotation allows us to convert terms from semantic domain back into
        syntax. This can be useful to find the normal form of a term, or when
        including terms from the semantics in the syntax during elaboration.

        The size parameter is the number of bindings present in the environment
        where we the resulting terms should be bound, allowing us to convert
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
    | FunApp of tm * tm list                 (** Function applications: [ f x ] *)


  (** {1 Elaboration } *)

  module Syntax = Core.Syntax
  module Semantics = Core.Semantics


  (** {2 Elaboration state} *)

  (** The elaboration context records the bindings that are currently bound at
      the current scope in the program. The environments are unzipped to make it
      more efficient to call functions from {!Core.Semantics}. *)
  type context = {
    size : Core.level;
    names : Core.name Semantics.env;
    tys : Semantics.tm Semantics.env;
    tms : Semantics.tm Semantics.env;
  }

  let initial_context = {
    size = 0;
    names = [];
    tys = [];
    tms = [];
  }

  let next_var context =
    Semantics.Neu (Semantics.Var context.size)

  let bind_def context name ty tm = {
    size = context.size + 1;
    names = name :: context.names;
    tys = ty :: context.tys;
    tms = tm :: context.tms;
  }

  let bind_param context name ty =
    bind_def context name ty (next_var context)

  let eval context : Syntax.tm -> Semantics.tm =
    Semantics.eval context.tms
  let quote context : Semantics.tm -> Syntax.tm =
    Semantics.quote context.size
  let is_convertible context : Semantics.tm * Semantics.tm -> bool =
    Semantics.is_convertible context.size
  let pretty context : Syntax.tm -> string =
    Syntax.pretty context.names
  let quote_pretty context tm : string =
    pretty context (quote context tm)

  (** {2 Exceptions} *)

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors or a missing type annotations. This is
      normal, and should be rendered nicely to the programmer. *)
  exception Error of string

  (** Raises an {!Error} exception *)
  let error message =
    raise (Error message)


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
              let context = bind_def context name (Lazy.force param_ty) var in
              Syntax.FunLit (name, go context names (body_ty var))
          | _, _ -> error "too many parameters in function literal"
        in
        go context names ty
    | tm, ty ->
        let tm, ty' = infer context tm in
        if is_convertible context (ty', ty) then tm else
          let expected = quote_pretty context ty in
          let found = quote_pretty context ty' in
          error ("type mismatch: expected `" ^ expected ^ "`, found `" ^ found ^ "`")

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
    | FunApp (head, args) ->
        List.fold_left
          (fun (head, head_ty) arg ->
            match head_ty with
            | Semantics.FunType (_, param_ty, body_ty) ->
                let arg = check context arg (Lazy.force param_ty) in
                (Syntax.FunApp (head, arg), body_ty (eval context arg))
            | _ -> error "not a function")
          (infer context head)
          args

  (* TODO: parser *)

end

(** Example terms for testing *)
module Examples = struct

  (* TODO: Implement a parser and convert to promote tests *)

  open Surface

  let stuff =
    Let ("Bool", None, FunType (["Out", Univ; "true", Name "Out"; "false", Name "Out"], Name "Out"),
    Let ("true", Some (Name "Bool"), FunLit (["Out"; "true"; "false"], Name "true"),
    Let ("false", Some (Name "Bool"), FunLit (["Out"; "true"; "false"], Name "false"),

    Let ("Option", Some (FunArrow (Univ, Univ)),
      FunLit (["A"], FunType (["Out", Univ; "some", FunArrow (Name "A", Name "Out"); "none", Name "Out"], Name "Out")),
    Let ("none", Some (FunType (["A", Univ], FunApp (Name "Option", [Name "A"]))),
      FunLit (["A"], FunLit (["Out"; "some"; "none"], Name "none")),
    Let ("some", Some (FunType (["A", Univ], FunArrow (Name "A", FunApp (Name "Option", [Name "A"])))),
      FunLit (["A"; "a"], FunLit (["Out"; "some"; "none"], FunApp (Name "some", [Name "a"]))),

      FunApp (Name "some", [FunApp (Name "Option", [Name "Bool"]);
        FunApp (Name "some", [Name "Bool"; Name "true"])])))))))

end

let () =
  let context = Surface.initial_context in
  let tm, ty = Surface.infer context Examples.stuff in
  print_endline ("  inferred type    │ " ^ Surface.quote_pretty context ty);
  print_endline ("  elaborated term  │ " ^ Surface.pretty context tm);
  print_endline ("  normalised term  │ " ^ Surface.quote_pretty context (Surface.eval context tm));
  print_endline ""
