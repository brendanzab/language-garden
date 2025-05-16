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
let level_to_index size level =
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
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * (vtm -> vtm)
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of level          (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of ntm * vtm
    | Bool_elim of ntm * vtm Lazy.t * vtm Lazy.t
    | Prim_app of Prim.t * vtm list


  (** {1 Eliminators} *)

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim (head : vtm) (vtm0 : vtm Lazy.t) (vtm1 : vtm Lazy.t) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm0, vtm1))
    | Bool_lit true -> Lazy.force vtm0
    | Bool_lit false -> Lazy.force vtm1
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (t1 = t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (t1 = t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 + t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 - t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 * t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (-t1)


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
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
    | Bool_elim (head, tm0, tm1) ->
        let head = eval env head in
        let vtm0 = lazy (eval env tm0) in
        let vtm1 = lazy (eval env tm1) in
        bool_elim head vtm0 vtm1
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : int) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : int) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Bool_elim (head, vtm0, vtm1) ->
        let tm0 = quote size (Lazy.force vtm0) in
        let tm1 = quote size (Lazy.force vtm1) in
        Bool_elim (quote_neu size head, tm0, tm1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

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

let rec pp_tm (names : name env) (fmt : Format.formatter) (tm : tm) : unit =
  match tm with
  | Let _ as tm ->
      let rec go names fmt tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<v>%a@]" (go names) tm
  | Fun_lit (name, param_ty, body) ->
      let rec go names fmt tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | Bool_elim (head, tm0, tm1) ->
      Format.fprintf fmt "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt tm =
  match tm with
  | Fun_app (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | Prim_app (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt tm =
  match tm with
  | Var index -> Format.fprintf fmt "%s" (List.nth names index)
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
