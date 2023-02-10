(** {0 Core language} *)

(** The core language is intended to be minimal, and close to well-understood
    type theories. The majority of this module is split up into the {!Syntax}
    and the {!Semantics}. *)


(** {1 Names} *)

(** Labels are significant to the equality of terms. They are typically used
    in the fields of records, and in record projections. *)
type label = string

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option


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

(** Converts a {!level} to an {!index} that is bound in an environment of the
    supplied size. Assumes that [ size > level ]. *)
let level_to_index size level =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by inverting a {!level} using {!level_to_index}. *)
type 'a env = 'a list


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

  let rec fun_lits = function
    | FunLit (name, body) ->
        let names, body = fun_lits body
        in name :: names, body
    | body -> [], body

  let fun_apps tm =
    let rec go args = function
      | FunApp (head, arg) -> go (arg :: args) head
      | head -> (head, args)
    in
    go [] tm

  let rec_projs tm =
    let rec go labels = function
      | RecProj (head, label) -> go (label :: labels) head
      | head -> (head, labels)
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
                Format.fprintf fmt "@[<2>@[let @[<2>%a@]@ :=@]@ @[%a;@]@]@ %a"
                  (pp_decl names) (name, def_ty)
                  (pp_tm names) def
                  (go (name :: names)) body
            | Let (name, def, body) ->
                Format.fprintf fmt "@[<2>@[let %a :=@]@ @[%a;@]@]@ %a"
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
            (pp_proj_tm names) head
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (pp_proj_tm names)) args
      | SingType (ty, sing_tm) ->
          Format.fprintf fmt "@[<2>%a@ @[[=@ %a]@]@]"
            (pp_proj_tm names) ty
            (pp_tm names) sing_tm
      | SingIntro tm ->
          Format.fprintf fmt "@[<2>#sing-intro@ %a@]"
            (pp_proj_tm names) tm
      | SingElim (tm, sing_tm) ->
          Format.fprintf fmt "@[<2>#sing-elim@ %a@ %a@]"
          (pp_proj_tm names) tm
          (pp_proj_tm names) sing_tm
      | tm ->
          pp_proj_tm names fmt tm

    and pp_proj_tm names fmt = function
      | RecProj (_, _) as tm ->
          let head, labels = rec_projs tm in
          Format.fprintf fmt "@[<2>%a@,%a@]"
            (pp_proj_tm names) head
            (Format.pp_print_list
              ~pp_sep:Format.pp_print_space
              (fun fmt label -> Format.fprintf fmt ".%s" label))
            labels
      | tm ->
          pp_atomic_tm names fmt tm

    and pp_atomic_tm names fmt = function
      | Var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
      | Univ -> Format.fprintf fmt "Type"
      | RecType Nil | RecLit [] -> Format.fprintf fmt "{}"
      | RecType decls ->
          (* TODO: Improve multiline formatting *)
          let rec go_decls names fmt = function
            | Nil -> Format.fprintf fmt ""
            | Cons (label, ty, Nil) ->
                Format.fprintf fmt "@[<2>%a@]"
                  (pp_decl names) (Some label, ty)
            | Cons (label, ty, decls) ->
                Format.fprintf fmt "@[<2>%a;@]@ %a"
                  (pp_decl names) (Some label, ty)
                  (go_decls (Some label :: names)) decls
          in
          Format.fprintf fmt "@[<2>{@ %a@ }@]"
            (go_decls names) decls
      | RecLit defns ->
          (* TODO: Improve multiline formatting *)
          let rec go_defns fmt = function
            | [] -> Format.fprintf fmt ""
            | (label, ty) :: [] ->
                Format.fprintf fmt "@[<2>%a@]"
                  (pp_defn names) (Some label, ty)
            | (label, ty) :: decls ->
                Format.fprintf fmt "@[<2>%a;@]@ %a"
                  (pp_defn names) (Some label, ty)
                  go_defns decls
          in
          Format.fprintf fmt "@[<2>{@ %a@ }@]"
            go_defns defns
      | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm

    and pp_decl names fmt (name, ty) =
      Format.fprintf fmt "@[%a@ :@]@ %a"
        pp_name name
        (pp_tm names) ty

    and pp_defn names fmt = function
      | name, Var index when resugar && name = List.nth names index ->
          Format.fprintf fmt "%a" pp_name name
      | name, tm ->
          Format.fprintf fmt "@[%a@ :=@]@ %a"
            pp_name name
            (pp_tm names) tm
    in

    pp_tm names

end

(** Semantics of the core language *)
module Semantics = struct

  (** {1 Semantic domain} *)

  (** The following data structures represent the semantic interpretation of
      the core syntax. *)

  (** Types *)
  type vty = vtm

  (** Terms in weak head normal form *)
  and vtm =
    | Neu of neu                          (** Neutral terms *)
    | Univ
    | FunType of name * vty * (vtm -> vty)
    | FunLit of name * (vtm -> vtm)
    | RecType of decls
    | RecLit of (label * vtm) list
    | SingType of vty * vtm
    | SingIntro                           (** Singleton introduction, with term erased *)

  (** Field declarations *)
  and decls =
    | Nil
    | Cons of label * vty * (vtm -> decls)

  (** Neutral terms are terms that could not be reduced to a normal form as a
      result of being stuck on something else that would not reduce further.
      I’m not sure why they are called ‘neutral terms’. Perhaps they are...
      ambivalent about what they might compute to? *)
  and neu =
    | Var of level                        (** Variable that could not be reduced further *)
    | FunApp of neu * vtm                 (** Function application *)
    | RecProj of neu * label              (** Record projection *)


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
  let rec eval tms : Syntax.tm -> vtm = function
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
  and quote_neu size tys : neu -> Syntax.tm * vty = function
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
  let rec is_convertible size tys tm1 tm2 : vty -> bool = function
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
