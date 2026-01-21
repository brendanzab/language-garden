(** {0 Core language} *)

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

(** To illustrate the relationship between indices and levels, the level and
    index of the variable [g] can be found as follows:

    {@text[
                    level = 3      index = 1
                │─λ───λ───λ──────▶︎│◀︎─λ──────│
                │                 │         │
      Names     │ λn. λf. λx. n (λg. λh. h (g f)) (λu. x) (λu. u)
      Indices   │ λ   λ   λ   2 (λ   λ   0 (1 3)) (λ   1) (λ   0)
      Levels    │ λ   λ   λ   0 (λ   λ   4 (3 1)) (λ   2) (λ   3)
    ]}

    Nameless variable representations like {i De Bruijn indices} and {i De
    Bruijn levels} have a reputation for off-by-one errors and requiring
    expensive re-indexing operations when performing substitutions. Thankfully
    these issues can be largely avoided by choosing different variable
    representations for the syntax and the semantic domain:

    - In the {i syntax} we represent bound variables with {!index}. This allows
      us to easily compare terms for alpha equivalence and quickly look up
      bindings based on their position in the environment.
    - In the {i semantic domain} we represent free variables with {!level}.
      Because the meaning of levels remains the same as new bindings are added
      to the environment, this lets us use terms at greater binding depths
      without needing to reindex them first.

    The only time we really need to reindex terms is when quoting from the
    syntax back to the semantic domain, using the {!level_to_index} function,
    and only requires a single traversal of the term.

    This approach is documented in more detail in Chapter 3 of Andreas Abel’s
    Thesis, {{: https://www.cse.chalmers.se/~abela/habil.pdf} “Normalization
    by Evaluation: Dependent Types and Impredicativity”}. Andras Kovacs also
    explains the approach in {{: https://proofassistants.stackexchange.com/a/910/309}
    an answer on the Proof Assistants Stack Exchange}.
*)

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


(** {1 Syntax} *)

(** Type syntax *)
type ty =
  | Var of index                    (* Local type variables *)
  | Forall_type of name * ty        (* Type of a forall (i.e. the type of a term parameterised by a type) *)
  | Fun_type of ty * ty             (* Type of function types *)
  | Int_type
  | Bool_type

(** Term syntax *)
type tm =
  | Var of index                    (* Local term variables *)
  | Prim of Prim.t
  | Let of name * ty * tm * tm
  | Forall_lit of name * tm         (* Forall literal (i.e. big-lambda abstraction) *)
  | Forall_app of tm * ty           (* Forall application *)
  | Fun_lit of name * ty * tm       (* Function literal (i.e. lambda abstraction) *)
  | Fun_app of tm * tm              (* Function application *)
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm

(** Apply a term to a list of arguments *)
let fun_app (head : tm) (args : tm list) : tm =
  List.fold_left (fun head arg -> Fun_app (head, arg)) head args


module Semantics = struct

  (** {1 Values} *)

  (** Types in weak head normal form (i.e. type values) *)
  type vty =
    | Var of level
    | Forall_type of name * (vty -> vty)
    | Fun_type of vty * vty
    | Int_type
    | Bool_type

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Prim_app of Prim.t * vtm list
    | Fun_lit of (vtm -> vtm)
    | Int_lit of int
    | Bool_lit of bool


  (** {1 Evaluation} *)

  (** Evaluate a type from the syntax into its semantic interpretation *)
  let rec eval_ty (ty_env : vty env) (ty : ty) : vty =
    match ty with
    | Var ty_index -> List.nth ty_env ty_index
    | Forall_type (name, body_ty) ->
        Forall_type (name, fun arg -> eval_ty (arg :: ty_env) body_ty)
    | Fun_type (param_ty, body_ty) ->
        let param_vty = eval_ty ty_env param_ty in
        let body_vty = eval_ty ty_env body_ty in
        Fun_type (param_vty, body_vty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval_tm (tm_env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var tm_index -> List.nth tm_env tm_index
    | Prim prim -> prim_app prim []
    | Let (_, _, def, body) ->
        let def = eval_tm tm_env def in
        eval_tm (def :: tm_env) body
    | Forall_lit (_, body) -> eval_tm tm_env body
    | Forall_app (head, _) -> eval_tm tm_env head
    | Fun_lit (_, _, body) ->
        Fun_lit (fun arg -> eval_tm (arg :: tm_env) body)
    | Fun_app (head, arg) ->
        begin match eval_tm tm_env head with
        | Fun_lit body -> body (eval_tm tm_env arg)
        | Prim_app (prim, args) -> prim_app prim ((eval_tm tm_env arg) :: args)
        | _ -> invalid_arg "expected function"
        end
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        begin match eval_tm tm_env head with
        | Bool_lit true -> eval_tm tm_env tm1
        | Bool_lit false -> eval_tm tm_env tm2
        | _ -> invalid_arg "expected boolean"
        end

  and prim_app (prim : Prim.t) (args : vtm list) : vtm =
    match prim, args with
    | Bool_eq, [Bool_lit t2; Bool_lit t1] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq, [Int_lit t2; Int_lit t1] -> Bool_lit (Int.equal t1 t2)
    | Int_add, [Int_lit t2; Int_lit t1] -> Int_lit (Int.add t1 t2)
    | Int_sub, [Int_lit t2; Int_lit t1] -> Int_lit (Int.sub t1 t2)
    | Int_mul, [Int_lit t2; Int_lit t1] -> Int_lit (Int.mul t1 t2)
    | Int_neg, [Int_lit t1] -> Int_lit (Int.neg t1)
    | prim, args -> Prim_app (prim, args)


  (** {1 Quotation} *)

  (** Convert types from the semantic domain back into syntax. *)
  let rec quote_vty  (ty_size : level) (vty : vty) : ty =
    match vty with
    | Var ty_level -> Var (level_to_index ty_size ty_level)
    | Forall_type (name, body_vty) ->
        let body = quote_vty (ty_size + 1) (body_vty (Var ty_size)) in
        Forall_type (name, body)
    | Fun_type (param_vty, body_vty) ->
        let param_ty = quote_vty ty_size param_vty in
        let body_ty = quote_vty ty_size body_vty in
        Fun_type (param_ty, body_ty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type


  (** {1 Conversion Checking} *)

  (** Checks that two types (in weak-head normal form) compute to the same type
      in normal form. This could be implemented naively by quoting both types
      and checking the resulting normal forms for alpha-equivalence, but it’s
      faster to compare the types in WHNF directly. *)
  let rec is_convertible (ty_size : level) (vty1 : vty) (vty2 : vty) : bool =
    match vty1, vty2 with
    | Var ty_level1, Var ty_level2 -> ty_level1 = ty_level2
    | Forall_type (_, body_ty1), Forall_type (_, body_ty2) ->
        let x : vty = Var ty_size in
        is_convertible (ty_size + 1) (body_ty1 x) (body_ty2 x)
    | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
        is_convertible ty_size param_ty1 param_ty2
          && is_convertible ty_size body_ty1 body_ty2
    | Int_type, Int_type -> true
    | Bool_type, Bool_type -> true
    | _, _ -> false


  (** {1 Pretty printing} *)

  let pp_vtm : vtm -> Format.formatter -> unit =
    let rec pp_vtm vtm ppf =
      match vtm with
      | Prim_app (prim, ((_ :: _) as args)) ->
          let pp_sep ppf () = Format.fprintf ppf "@ " in
          Format.fprintf ppf "@[<2>#%s %t@]"
            (Prim.name prim)
            (Fun.flip (Format.pp_print_list (Fun.flip pp_atomic_vtm) ~pp_sep) args)
      | vtm ->
          pp_atomic_vtm vtm ppf
    and pp_atomic_vtm vtm ppf =
      match vtm with
      | Prim_app (prim, []) -> Format.fprintf ppf "#%s" (Prim.name prim)
      | Fun_lit _ -> Format.fprintf ppf "<function>"
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Int_lit i -> Format.fprintf ppf "%i" i
      | Prim_app (_, (_ :: _)) as vtm ->
          Format.fprintf ppf "@[(%t)@]" (pp_vtm vtm)
    in
    pp_vtm

end


(** {1 Pretty printing} *)

let pp_name (name : name) (ppf : Format.formatter) : unit =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_ty : name env -> ty -> Format.formatter -> unit =
  let rec pp_ty ty_names ty ppf =
    match ty with
    | Forall_type (name, body_ty) ->
        Format.fprintf ppf "[%t] -> %t"
          (pp_name name)
          (pp_ty (name :: ty_names) body_ty)
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty ty_names param_ty)
          (pp_ty ty_names body_ty)
    | ty ->
        pp_atomic_ty ty_names ty ppf
  and pp_atomic_ty (ty_names : name env) (ty : ty) (ppf : Format.formatter) =
    match ty with
    | Var ty_index -> Format.fprintf ppf "%t" (pp_name (List.nth ty_names ty_index))
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Forall_type _ | Fun_type _ as ty ->
        Format.fprintf ppf "@[(%t)@]" (pp_ty ty_names ty)
  in
  pp_ty

let pp_name_ann (ty_names : name env) (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (pp_ty ty_names ty)

let pp_param (ty_names : name env) (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]" (pp_name name) (pp_ty ty_names ty)

let pp_tm : name env -> name env -> tm -> Format.formatter -> unit =
  let rec pp_tm ty_names tm_names tm ppf =
    let rec go_params ty_names tm_names (tm : tm) ppf =
      match tm with
      | Forall_lit (name, body) ->
          Format.fprintf ppf "@ @[fun@ [%t]@ =>@]%t"
            (pp_name name)
            (go_params (name :: ty_names) tm_names body)
      | Fun_lit (name, param_ty, body) ->
          Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
            (pp_param ty_names name param_ty)
            (go_params ty_names (name :: tm_names) body)
      | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm ty_names tm_names tm)
    in
    match tm with
    | Let _ as tm ->
        let rec go tm_names (tm : tm) ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann ty_names name def_ty)
                (pp_tm ty_names tm_names def)
                (go (name :: tm_names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm ty_names tm_names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go tm_names tm)
    | Forall_lit (name, body) ->
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ [%t]@ =>@]%t"
          (pp_name name)
          (go_params (name :: ty_names) tm_names body)
    | Fun_lit (name, param_ty, body) ->
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
          (pp_param ty_names name param_ty)
          (go_params ty_names (name :: tm_names) body)
    | Bool_elim (head, tm1, tm2) ->
        Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_tm ty_names tm_names head)
          (pp_app_tm ty_names tm_names tm1)
          (pp_tm ty_names tm_names tm2)
    | tm ->
        pp_app_tm ty_names tm_names tm ppf
  and pp_app_tm ty_names tm_names tm ppf =
    let rec go tm ppf =
      match tm with
      | Forall_app (head, arg) ->
          Format.fprintf ppf "%t@ [%t]"
            (go head)
            (pp_ty ty_names arg)
      | Fun_app (head, arg) ->
          Format.fprintf ppf "%t@ %t"
            (go head)
            (pp_atomic_tm ty_names tm_names arg)
      | tm ->
          pp_atomic_tm ty_names tm_names tm ppf
    in
    match tm with
    | Forall_app _ | Fun_app _ as tm ->
        Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
    | tm ->
        pp_atomic_tm ty_names tm_names tm ppf
  and pp_atomic_tm ty_names tm_names tm ppf =
    match tm with
    | Var tm_index -> Format.fprintf ppf "%t" (pp_name (List.nth tm_names tm_index))
    | Prim prim -> Format.fprintf ppf "#%s" (Prim.name prim)
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Let _ | Forall_lit _ | Forall_app _ | Fun_lit _ | Fun_app _
    | Bool_elim _ ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm ty_names tm_names tm)
  in
  pp_tm
