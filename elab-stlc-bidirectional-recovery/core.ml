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
  | Fun_type of ty * ty
  | Int_type
  | Bool_type
  | Reported_error              (* Error sentinel *)

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
  | Reported_error              (* Error sentinel *)


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
    | Var of level                  (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of ntm * vtm
    | Bool_elim of ntm * (unit -> vtm) * (unit -> vtm)
    | Prim_app of Prim.t * vtm list
    | Reported_error                (* Error sentinel *)


  (** {1 Eliminators} *)

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim (head : vtm) (vtm1 : unit -> vtm) (vtm2 : unit -> vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> vtm1 ()
    | Bool_lit false -> vtm2 ()
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (Int.equal t1 t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.add t1 t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.sub t1 t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.mul t1 t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (Int.neg t1)


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
    | Bool_elim (head, tm1, tm2) ->
        let head = eval env head in
        let vtm1 () = eval env tm1 in
        let vtm2 () = eval env tm2 in
        bool_elim head vtm1 vtm2
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | Reported_error ->
        Neu Reported_error


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm1 = quote size (vtm1 ()) in
        let tm2 = quote size (vtm2 ()) in
        Bool_elim (quote_neu size head, tm1, tm2)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote size) args)
    | Reported_error ->
        Reported_error


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

end


(** {1 Pretty printing} *)

let pp_ty : ty -> Format.formatter -> unit =
  let rec pp_ty ty ppf =
    match ty with
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty param_ty)
          (pp_ty body_ty)
    | ty ->
        pp_atomic_ty ty ppf
  and pp_atomic_ty ty ppf =
    match ty with
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Reported_error -> Format.fprintf ppf "_"
    | Fun_type _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
  in
  pp_ty

let pp_name_ann (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%s :@]@ %t@]" name (pp_ty ty)

let pp_param (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%s :@]@ %t)@]" name (pp_ty ty)

let pp_tm : name env -> tm -> Format.formatter -> unit =
  let rec pp_tm names tm ppf =
    match tm with
    | Let _ as tm ->
        let rec go names tm ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_tm names def)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go names tm)
    | Fun_lit (name, param_ty, body) ->
        let rec go names tm ppf =
          match tm with
          | Fun_lit (name, param_ty, body) ->
              Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
                (pp_param name param_ty)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
          (pp_param name param_ty)
          (go (name :: names) body)
    | Bool_elim (head, tm1, tm2) ->
        Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_tm names head)
          (pp_app_tm names tm1)
          (pp_tm names tm2)
    | tm ->
        pp_app_tm names tm ppf
  and pp_app_tm names tm ppf =
    match tm with
    | Fun_app (head, arg) ->
        Format.fprintf ppf "@[%t@ %t@]"
          (pp_app_tm names head)
          (pp_atomic_tm names arg)
    | Prim_app (prim, args) ->
        let pp_sep ppf () = Format.fprintf ppf "@ " in
        Format.fprintf ppf "@[#%s@ %t@]"
          (Prim.name prim)
          (fun ppf ->
            Format.pp_print_list ~pp_sep (Fun.flip (pp_atomic_tm names)) ppf args)
    | tm ->
        pp_atomic_tm names tm ppf
  and pp_atomic_tm names tm ppf =
    match tm with
    | Var index -> Format.fprintf ppf "%s" (List.nth names index)
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Reported_error -> Format.fprintf ppf "#reported-error"
    | Let _ | Fun_lit _ | Fun_app _ | Bool_elim _ | Prim_app _ as tm ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)
  in
  pp_tm
