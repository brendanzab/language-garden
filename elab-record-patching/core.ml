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

  (** Types *)
  type ty = tm

  (** Terms *)
  and tm =
    | Let of name * ty * tm * tm
    | Var of index
    | Univ
    | Fun_type of name * ty * ty
    | Fun_lit of name * tm
    | Fun_app of tm * tm
    | Rec_type of (label * ty) list
    | Rec_lit of (label * tm) list
    | Rec_proj of tm * label
    | Sing_type of ty * tm              (** Singleton type former: [ A [= x ] ] *)
    | Sing_intro                        (** Singleton introduction: [ #sing-intro ] *)

  (** Each ‘connective’ in the core language follows a similar pattern, with
      separate variants for:

      - type formation: for constructing types that represent the connective
      - introduction: for constructing terms of a given connective
      - elimination: for deconstructing terms of a given connective
  *)


  (** Returns [ true ] if the variable is bound anywhere in the term. *)
  let rec is_bound (var : index) (tm : tm) : bool =
    match tm with
    | Let (_, def_ty, def, body) ->
        is_bound var def_ty || is_bound var def || is_bound (var + 1) body
    | Var index -> index = var
    | Univ -> false
    | Fun_type (_, param_ty, body_ty) -> is_bound var param_ty || is_bound (var + 1) body_ty
    | Fun_lit (_, body) -> is_bound (var + 1) body
    | Fun_app (head, arg) -> is_bound var head || is_bound var arg
    | Rec_type decls -> is_bound_decls var decls
    | Rec_lit defns -> List.exists (fun (_, tm) -> is_bound var tm) defns
    | Rec_proj (head, _) -> is_bound var head
    | Sing_type (ty, sing_tm) -> is_bound var ty || is_bound var sing_tm
    | Sing_intro -> false
  and is_bound_decls (var : index) (decls : (label * ty) list) =
    match decls with
    | [] -> false
    | (_, ty) :: decls ->
        is_bound var ty || is_bound_decls (var + 1) decls

  let fun_apps (tm : tm) : ty * ty list =
    let rec go args tm =
      match tm with
      | Fun_app (head, arg) -> go (arg :: args) head
      | head -> (head, args)
    in
    go [] tm

  let rec_projs (tm : tm) : ty * label env =
    let rec go labels tm =
      match tm with
      | Rec_proj (head, label) -> go (label :: labels) head
      | head -> (head, labels)
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
      | Let (_, _, _, _) as tm ->
          let rec go names tm ppf =
            match tm with
            | Let (name, def_ty, def, body) ->
                Format.fprintf ppf "@[<2>@[let @[<2>%t@]@ :=@]@ @[%t;@]@]@ %t"
                  (pp_decl names name def_ty)
                  (pp_tm names def)
                  (go (name :: names) body)
            (* Final term should be grouped in a box *)
            | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
          in
          Format.fprintf ppf "@[<v>%t@]" (go names tm)
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
                Format.fprintf ppf "%t@ %t"
                  (pp_param names name param_ty)
                  (go (name :: names) body_ty)
            (* Open record types on same line as parameter list *)
            | Rec_type (_ :: _ as decls) ->
                Format.fprintf ppf "@[<hv>@[->@ {@]%t@ }@]"
                  (pp_decls names decls)
            | body_ty ->
                (* TODO: improve printing of record types *)
                Format.fprintf ppf "@[->@ @[%t@]@]"
                  (pp_tm names body_ty)
          in
          Format.fprintf ppf "@[<4>fun %t@]" (go names tm)
      | Fun_lit (name, body) ->
          let rec go names tm ppf =
            match tm with
            | Fun_lit (name, body) ->
                Format.fprintf ppf "%t@ %t"
                  (pp_name name)
                  (go (name :: names) body)
            (* Open record types and literals on same line as parameter list *)
            | Rec_type (_ :: _ as decls) ->
                Format.fprintf ppf "=>@ {@]%t@ }@]"
                  (pp_decls names decls)
            | Rec_lit (_ :: _ as defns) ->
                Format.fprintf ppf "=>@ {@]%t@ }@]"
                  (pp_defns names defns)
            | tm ->
                Format.fprintf ppf "=>@]@;<1 2>@[%t@]@]"
                  (pp_tm names tm)
          in
          Format.fprintf ppf "@[<hv>@[fun@ %t@ %t"
            (pp_name name)
            (go (name :: names) body)
      | tm ->
          pp_app_tm names tm ppf

    and pp_app_tm names tm ppf =
      match tm with
      | Fun_app (_, _) as tm ->
          let head, args = fun_apps tm in
          Format.fprintf ppf "@[<2>%t@ %a@]"
            (pp_proj_tm names head)
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (Fun.flip (pp_proj_tm names))) args
      | Sing_type (ty, sing_tm) ->
          Format.fprintf ppf "@[<2>%t@ @[[=@ %t]@]@]"
            (pp_proj_tm names ty)
            (pp_tm names sing_tm)
      | Sing_intro ->
          Format.fprintf ppf "#sing-intro"
      | tm ->
          pp_proj_tm names tm ppf

    and pp_proj_tm names tm ppf =
      match tm with
      | Rec_proj (_, _) as tm ->
          let head, labels = rec_projs tm in
          Format.fprintf ppf "@[<2>%t@,%a@]"
            (pp_proj_tm names head)
            (Format.pp_print_list
              ~pp_sep:Format.pp_print_space
              (fun ppf label -> Format.fprintf ppf ".%s" label))
            labels
      | tm ->
          pp_atomic_tm names tm ppf

    and pp_atomic_tm names tm ppf =
      match tm with
      | Var index -> Format.fprintf ppf "%t" (pp_name (List.nth names index))
      | Univ -> Format.fprintf ppf "Type"
      | Rec_type [] | Rec_lit [] -> Format.fprintf ppf "{}"
      | Rec_type decls ->
          Format.fprintf ppf "@[<hv>{%t@ }@]"
            (pp_decls names decls)
      | Rec_lit defns ->
          Format.fprintf ppf "@[<hv>{%t@ }@]"
            (pp_defns names defns)
      | Let _ | Fun_type _ | Fun_lit _ | Fun_app _ | Rec_proj _ | Sing_type _ | Sing_intro ->
          Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)

    and pp_decl names name ty ppf =
      Format.fprintf ppf "@[%t@ :@]@ %t"
        (pp_name name)
        (pp_tm names ty)

    and pp_defn names name tm ppf =
      match tm with
      | Var index when resugar && name = List.nth names index ->
          Format.fprintf ppf "%t" (pp_name name)
      (* TODO: start function literals  on same line *)
      | tm ->
          Format.fprintf ppf "@[%t@ :=@]@ %t"
            (pp_name name)
            (pp_tm names tm)

    and pp_decls names tm ppf =
      match tm with
      | [] -> Format.fprintf ppf ""
      | [label, ty] ->
          (* TODO: use trailing semicolons when splitting over multiple lines *)
          Format.fprintf ppf "@;<1 2>@[<2>%t@]"
            (pp_decl names (Some label) ty)
      | (label, ty) :: decls ->
          Format.fprintf ppf "@;<1 2>@[<2>%t;@]%t"
            (pp_decl names (Some label) ty)
            (pp_decls (Some label :: names) decls)

    and pp_defns names tm ppf =
      match tm with
      | [] -> Format.fprintf ppf ""
      | (label, ty) :: [] ->
          (* TODO: use trailing semicolons when splitting over multiple lines *)
          Format.fprintf ppf "@;<1 2>@[<2>%t@]"
            (pp_defn names (Some label) ty)
      | (label, ty) :: decls ->
          Format.fprintf ppf "@;<1 2>@[<2>%t;@]%t"
            (pp_defn names (Some label) ty)
            (pp_defns names decls)

    and pp_param names name def_ty ppf =
      Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]"
        (pp_name name)
        (pp_tm names def_ty)
    in

    pp_tm

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
    | Fun_type of name * vty * (vtm -> vty)
    | Fun_lit of name * (vtm -> vtm)
    | Rec_type of decls
    | Rec_lit of (label * vtm) list
    | Sing_type of vty * vtm
    | Sing_intro                          (** Singleton introduction *)

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
    | Fun_app of neu * vtm                (** Function application *)
    | Rec_proj of neu * label             (** Record projection *)


  (** {1 Error handling} *)

  (** An error that was encountered during computation. This should only ever
      be raised if ill-typed terms were supplied to the semantics. *)
  exception Error of string

  (** Raises an {!Error} exception *)
  let error (message : string) =
    raise (Error message)


  (** {1 Eliminators} *)

  (** The following functions trigger computation if the head term is in the
      appropriate normal form, otherwise queuing up the elimination if the
      term is in a neutral form. *)

  (** Compute a function application *)
  let app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu neu -> Neu (Fun_app (neu, arg))
    | Fun_lit (_, body) -> body arg
    | _ -> error "invalid application"

  (** Compute a record projection *)
  let proj (head : vtm) (label : label) : vtm =
    match head with
    | Rec_lit defns -> defns |> List.find (fun (l, _) -> l = label) |> snd
    | Neu neu -> Neu (Rec_proj (neu, label))
    | _ -> error "invalid projection"


  (** {1 Finding the types of record projections} *)

  (** Returns the type of a record projection *)
  let proj_ty (head : vtm) (decls : decls) (label : label) : vty option =
    let rec go decls =
      match decls with
        | Nil -> None
        | Cons (l, ty, _) when l = label -> Some ty
        | Cons (l, _, decls) -> go (decls (proj head l))
    in
    go decls


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (tms : vtm env) (tm : Syntax.tm) : vtm =
    match tm with
    | Let (_, _, def, body) -> eval (eval tms def :: tms) body
    | Var index -> List.nth tms index
    | Univ -> Univ
    | Fun_type (name, param_ty, body_ty) ->
        Fun_type (name, eval tms param_ty, fun x -> eval (x :: tms) body_ty)
    | Fun_lit (name, body) -> Fun_lit (name, fun x -> eval (x :: tms) body)
    | Fun_app (head, arg) -> app (eval tms head) (eval tms arg)
    | Rec_type decls -> Rec_type (eval_decls tms decls)
    | Rec_lit defns ->
        Rec_lit (List.map (fun (label, expr) -> (label, eval tms expr)) defns)
    | Rec_proj (head, label) -> proj (eval tms head) label
    | Sing_type (ty, sing_tm) -> Sing_type (eval tms ty, eval tms sing_tm)
    | Sing_intro -> Sing_intro
  and eval_decls (tms : vtm env) (decls : (label * Syntax.ty) list) : decls =
    match decls with
    | [] -> Nil
    | (label, ty) :: decls ->
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
  *)
  let rec quote (size : level) (tm : vtm) : Syntax.tm =
    match tm with
    | Neu neu -> quote_neu size neu
    | Univ -> Univ
    | Fun_type (name, param_ty, body_ty) ->
        let param_ty = quote size param_ty in
        let body_ty = quote (size + 1) (body_ty (Neu (Var size))) in
        Fun_type (name, param_ty, body_ty)
    | Fun_lit (name, body) ->
        Fun_lit (name, quote (size + 1) (body (Neu (Var size))))
    | Rec_type decls -> Rec_type (quote_decls size decls)
    | Rec_lit defns -> Rec_lit (defns |> List.map (fun (label, tm) -> (label, quote size tm)))
    | Sing_type (ty, sing_tm) -> Sing_type (quote size ty, quote size sing_tm)
    | Sing_intro -> Sing_intro
  and quote_neu (size : level) (neu : neu) : Syntax.tm =
    match neu with
    | Var level -> Syntax.Var (level_to_index size level)
    | Fun_app (head, arg) -> Syntax.Fun_app (quote_neu size head, quote size arg)
    | Rec_proj (head, label) -> Syntax.Rec_proj (quote_neu size head, label)
  and quote_decls (size : level) (decls : decls) : (label * Syntax.ty) list =
    match decls with
    | Nil -> []
    | Cons (label, ty, decls) ->
        (label, quote size ty) :: quote_decls (size + 1) (decls (Neu (Var size)))


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (size : level) (tms : vtm env) (tm : Syntax.tm) : Syntax.tm =
    quote size (eval tms tm)


  (** {1 Conversion Checking} *)

  (** Checks that two values compute to the same term. This could be implemented
      naively by quoting both values and checking the resulting terms for
      alpha-equivalence, but it’s faster to compare the values directly.

      A precondition of this function is that both values share the same type.
      This allows us to support cheap, syntax directed eta conversion for
      functions and best-effort eta conversion for singletons and unit records,
      similar to the approach found in {{: https://github.com/AndrasKovacs/elaboration-zoo/blob/master/03-holes-unit-eta}
      elaboration-zoo/03-holes-unit-eta}. *)
  let rec is_convertible (size : level) (tm1 : vtm) (tm2 : vtm) : bool =
    match tm1, tm2 with
    | Neu n1, Neu n2 -> is_convertible_neu size n1 n2
    | Univ, Univ -> true
    | Fun_type (_, param_ty1, body_ty1), Fun_type (_, param_ty2, body_ty2) ->
        let var = Neu (Var size) in
        is_convertible size param_ty1 param_ty2
          && is_convertible (size + 1) (body_ty1 var) (body_ty2 var)
    | Fun_lit (_, body1), Fun_lit (_, body2) ->
        let x = Neu (Var size) in
        is_convertible (size + 1) (body1 x) (body2 x)
    | Rec_type decls1, Rec_type decls2 ->
        is_convertible_decls size decls1 decls2
    | Sing_type (ty1, sing_tm1), Sing_type (ty2, sing_tm2) ->
        is_convertible size ty1 ty2
          && is_convertible size sing_tm1 sing_tm2
    | Sing_intro, Sing_intro -> true

    (* Eta rules *)
    | Fun_lit (_, body), fun_tm | fun_tm, Fun_lit (_, body)  ->
        let x = Neu (Var size) in
        is_convertible size (body x) (app fun_tm x)
    | Rec_lit decls, rec_tm | rec_tm, Rec_lit decls ->
        decls |> List.for_all (fun (label, elem) -> is_convertible size elem (proj rec_tm label))
    | Sing_intro, _ | _, Sing_intro -> true

    | _, _ -> false
  and is_convertible_neu (size : level) (neu1 : neu) (neu2 : neu) =
    match neu1, neu2 with
    | Var level1, Var level2 -> level1 = level2
    | Fun_app (func1, arg1), Fun_app (func2, arg2) ->
        is_convertible_neu size func1 func2 && is_convertible size arg1 arg2
    | Rec_proj (record1, label1), Rec_proj (record2, label2) ->
        label1 = label2 && is_convertible_neu size record1 record2
    | _, _ -> false
  and is_convertible_decls (size : level) (decls1 : decls) (decls2 : decls) =
    match decls1, decls2 with
    | Nil, Nil -> true
    | Cons (label1, ty1, decls1), Cons (label2, ty2, decls2) when label1 = label2 ->
        let var = Neu (Var size) in
        is_convertible size ty1 ty2
          && is_convertible_decls (size + 1) (decls1 var) (decls2 var)
    | _, _ -> false

end
