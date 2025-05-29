(** {0 Core language} *)

(** {1 Names} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string


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

(** Converts a {!level} to an {!index} that is bound in an environment of the
    supplied size. Assumes that [ size > level ]. *)
let level_to_index (size : level) (level : level) =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by inverting a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Syntax} *)

(** Type syntax *)
type ty =
  | Fun_type of ty * ty
  | Int_type
  | Bool_type

(** Term syntax *)
type expr =
  | Var of index
  | Let of name * ty * expr * expr
  | Fun_lit of name * ty * expr
  | Fun_app of expr * expr
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of expr * expr * expr
  | Prim_app of Prim.t * expr list


module Semantics = struct

  (** {1 Values} *)

  (** Expressions in weak head normal form (i.e. values) *)
  type vexpr =
    | Neu of nexpr
    | Fun_lit of name * ty * (vexpr -> vexpr)
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} expressions back to syntax, which is useful
      for pretty printing under binders.
  *)
  and nexpr =
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of nexpr * vexpr
    | Bool_elim of nexpr * vexpr Lazy.t * vexpr Lazy.t
    | Prim_app of Prim.t * vexpr list


  (** {1 Eliminators} *)

  let fun_app head arg =
    match head with
    | Neu nexpr -> Neu (Fun_app (nexpr, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim head vexpr0 vexpr1 =
    match head with
    | Neu nexpr -> Neu (Bool_elim (nexpr, vexpr0, vexpr1))
    | Bool_lit true -> Lazy.force vexpr0
    | Bool_lit false -> Lazy.force vexpr1
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vexpr list -> vexpr =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit e1; Bool_lit e2] -> Bool_lit (e1 = e2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit e1; Int_lit e2] -> Bool_lit (e1 = e2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit e1; Int_lit e2] -> Int_lit (e1 + e2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit e1; Int_lit e2] -> Int_lit (e1 - e2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit e1; Int_lit e2] -> Int_lit (e1 * e2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit e1] -> Int_lit (-e1)


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vexpr env) (e : expr) : vexpr =
    match e with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (name, param_ty, body) ->
        Fun_lit (name, param_ty, fun arg -> eval (arg :: env) body)
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, e0, e1) ->
        let head = eval env head in
        let ve0 = lazy (eval env e0) in
        let ve1 = lazy (eval env e1) in
        bool_elim head ve0 ve1
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (ve : vexpr) : expr =
    match ve with
    | Neu ne -> quote_neu size ne
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : level) (ne : nexpr) : expr =
    match ne with
    | Var level ->
        Var (level_to_index size level)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Bool_elim (head, vexpr0, vexpr1) ->
        let e0 = quote size (Lazy.force vexpr0) in
        let e1 = quote size (Lazy.force vexpr1) in
        Bool_elim (quote_neu size head, e0, e1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vexpr list) (e : expr) : expr =
    quote (List.length env) (eval env e)

end


(** {1 Pretty printing} *)

let rec pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt ty =
  match ty with
  | Int_type -> Format.fprintf fmt "Int"
  | Bool_type -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_name_ann fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let rec pp_expr (names : name env) (fmt : Format.formatter) (e : expr) : unit =
  match e with
  | Let _ as e ->
      let rec go names fmt e =
        match e with
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_expr names) def
              (go (name :: names)) body
        | e -> Format.fprintf fmt "@[%a@]" (pp_expr names) e
      in
      Format.fprintf fmt "@[<v>%a@]" (go names) e
  | Fun_lit (name, param_ty, body) ->
      let rec go names fmt e =
        match e with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | e -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_expr names) e
      in
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | Bool_elim (head, e0, e1) ->
      Format.fprintf fmt "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_expr names) head
        (pp_app_expr names) e0
        (pp_expr names) e1
  | e ->
      pp_app_expr names fmt e
and pp_app_expr names fmt e =
  match e with
  | Fun_app (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_expr names) head
        (pp_atomic_expr names) arg
  | Prim_app (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_expr names)) args
  | e ->
      pp_atomic_expr names fmt e
and pp_atomic_expr names fmt e =
  match e with
  | Var index -> Format.fprintf fmt "%s" (List.nth names index)
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | e -> Format.fprintf fmt "@[(%a)@]" (pp_expr names) e
