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
  (** The core language is simple and minimal, and is intended to be close to
      well-understood type theories. *)

  (** Names that serve as hints when pretty printing binders and variables, but
      should not impact the equality of terms. *)
  type name = string

  (** Syntax of the core language *)
  module Syntax = struct

    (** De-bruijn index *)
    type index = int

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

  end

  (** Semantics of the core language *)
  module Semantics = struct

    (** {1 Semantic domain} *)

    (** De-bruijn level *)
    type level = int

    (** Types *)
    type ty = tm

    (** Terms in weak head normal form *)
    and tm =
      | Neu of neu                            (** Neutral terms *)
      | Univ
      | FunType of string * ty * (tm -> ty)
      | FunLit of string * (tm -> tm)

    (** Neutral terms

        These are terms that could not be reduced to a normal form as a result
        of being stuck on something else that would not reduce further.

        I’m not sure why they are called ‘neutral terms’. Perhaps they are...
        ambivalent about what they might evaluate to?
    *)
    and neu =
      | Var of level          (** Variable that could not be reduced further *)
      | FunApp of neu * tm    (** Function application *)

    (** An environment of bindings that can be looked up directly using a
        {!Syntax.index}, or by inverting a {!level} using [ size - level - 1 ],
        where [ size ] is the number of bindings in the environment. *)
    type 'a env = 'a list


    (** {1 Exceptions} *)

    (** Internal error encountered in the semantics due to malformed terms being
        supplied to the semantics. *)
    exception Error of string


    (** {1 Eliminators} *)

    (** The following functions trigger computation if the head term is in the
        appropriate normal form, otherwise queuing up the elimination if the
        term is in a neutral form. *)

    (** Compute a function application *)
    let app head arg =
      match head with
      | Neu neu -> Neu (FunApp (neu, arg))
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
          FunType (name, eval env param_ty, fun x -> eval (x :: env) body_ty)
      | Syntax.FunLit (name, body) -> FunLit (name, fun x -> eval (x :: env) body)
      | Syntax.FunApp (head, arg) -> app (eval env head) (eval env arg)


    (** {1 Quotation} *)

    (** Quotation allows us to turn the semantic domain back into syntax. This
        is useful if we want to find the normal form of a term, or if we want to
        include a semantic term in some other bit of syntax. *)
    let rec quote size = function
      | Neu neu -> quote_neu size neu
      | Univ -> Syntax.Univ
      | FunType (name, param_ty, body_ty) ->
          let x = Neu (Var size) in
          Syntax.FunType (name, quote size param_ty, quote (size + 1) (body_ty x))
      | FunLit (name, body) ->
          let x = Neu (Var size) in
          Syntax.FunLit (name, quote (size + 1) (body x))
    and quote_neu size = function
      | Var level -> Syntax.Var (size - level - 1)
      | FunApp (neu, arg) -> Syntax.FunApp (quote_neu size neu, quote size arg)


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
          is_convertible size (param_ty1, param_ty2)
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
          is_convertible_neu size (neu1, neu2) && is_convertible size (arg1, arg2)
      | _, _ -> false


    (** {1 Pretty printing} *)

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
      and go_neu wrap size names = function
        | Var level -> str (List.nth names (size - level - 1))
        | FunApp (head, arg) ->
            parens wrap (go_neu false size names head << str " " <<  go true size names arg)
      in
      go false size names tm ""

  end
end


(** Surface language *)
module Surface = struct

  (** {1 Surface Syntax} *)

  (** Terms in the surface language *)
  type tm =
    | Let of string * tm option * tm * tm
    | Ann of tm * tm
    | Name of string
    | Univ
    | FunType of (string * tm) list * tm
    | FunArrow of tm * tm
    | FunLit of string list * tm
    | FunApp of tm * tm list


  (** {1 Elaboration } *)

  module Syntax = Core.Syntax
  module Semantics = Core.Semantics


  (** {2 Elaboration state} *)

  (** The elaboration context records the bindings that are currently bound at
      the current scope in the program. The environments are unzipped to make it
      more efficient to call functions from {!Core.Semantics}. *)
  type context = {
    size : Semantics.level;
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
  let pretty context : Semantics.tm -> string =
    Semantics.pretty context.size context.names

  (** {2 Exceptions} *)

  (** An elaboration error that will be raised if there was a problem in the
      surface syntax, usually as a result of type errors or a lack of type
      annotations. This is normal, and should be rendered nicely to the
      programmer. *)
  exception Error of string


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
    | tm, ty ->
        let tm, ty' = infer context tm in
        if is_convertible context (ty', ty) then tm else
          let expected = pretty context ty in
          let found = pretty context ty' in
          raise (Error ("type mismatch: expected `" ^ expected ^ "`, found `" ^ found ^ "`"))

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
    | FunApp (head, args) ->
        List.fold_left
          (fun (head, head_ty) arg ->
            match head_ty with
            | Semantics.FunType (_, param_ty, body_ty) ->
                let arg = check context arg param_ty in
                (Syntax.FunApp (head, arg), body_ty (eval context arg))
            | _ -> raise (Error "not a function"))
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
  print_endline ("  inferred type   │ " ^ Surface.pretty context ty);
  print_endline ("  evaluated term  │ " ^ Surface.pretty context (Surface.eval context tm));
  print_endline ""
