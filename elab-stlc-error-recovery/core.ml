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
  | Unknown_type            (* A type hole, used when recovering from errors *)

(** Term syntax *)
type tm =
  | Var of index
  | Prim of Prim.t
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Reported_error


module Semantics = struct

  (** {1 Values} *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * clos
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of level
    | Prim_app of Prim.t * vtm list
    | Fun_app of ntm * vtm
    | Bool_elim of ntm * thunk * thunk
    | Reported_error

  (** Closures that can be instantiated with a value. The environment provides
      a value for each variable in the term, except for the variable that the
      closure will be instantiated with during evaluation. *)
  and clos =
    | Clos of vtm list * tm

  (** A delayed computation, used in the branches of conditionals. *)
  and thunk =
    | Thunk of vtm list * tm


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Prim prim -> prim_app prim []
    | Let (_, _, def, body) ->
        eval (eval env def :: env) body
    | Fun_lit (name, param_ty, body) ->
        Fun_lit (name, param_ty, Clos (env, body))
    | Fun_app (head, arg) ->
        fun_app (eval env head) (eval env arg)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        bool_elim (eval env head) (Thunk (env, tm1)) (Thunk (env, tm2))
    | Reported_error ->
        Neu Reported_error

  (** Instantiate a closure with a value *)
  and inst_clos (Clos (env, body) : clos) (arg : vtm) : vtm =
    eval (arg :: env) body

  (** Force the computation of a thunk *)
  and force_thunk (Thunk (env, body) : thunk) : vtm =
    eval env body


  (** {1 Eliminators} *)

  and prim_app (prim : Prim.t) (args : vtm list) : vtm =
    match prim, args with
    | Bool_eq, [Bool_lit t2; Bool_lit t1] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq, [Int_lit t2; Int_lit t1] -> Bool_lit (Int.equal t1 t2)
    | Int_add, [Int_lit t2; Int_lit t1] -> Int_lit (Int.add t1 t2)
    | Int_sub, [Int_lit t2; Int_lit t1] -> Int_lit (Int.sub t1 t2)
    | Int_mul, [Int_lit t2; Int_lit t1] -> Int_lit (Int.mul t1 t2)
    | Int_neg, [Int_lit t1] -> Int_lit (Int.neg t1)
    | prim, args -> Neu (Prim_app (prim, args))

  and fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu (Prim_app (prim, args)) -> prim_app prim (arg :: args)
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> inst_clos body arg
    | _ -> invalid_arg "expected function"

  and bool_elim (head : vtm) (vtm1 : thunk) (vtm2 : thunk) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> force_thunk vtm1
    | Bool_lit false -> force_thunk vtm2
    | _ -> invalid_arg "expected boolean"


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (inst_clos body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_ntm (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | Prim_app (prim, []) -> Prim prim
    | Prim_app (prim, arg :: args) ->
        Fun_app (quote_ntm size (Prim_app (prim, args)), quote size arg)
    | Fun_app (head, arg) ->
        Fun_app (quote_ntm size head, quote size arg)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm1 = quote size (force_thunk vtm1) in
        let tm2 = quote size (force_thunk vtm2) in
        Bool_elim (quote_ntm size head, tm1, tm2)
    | Reported_error ->
        Reported_error


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

end


(** {1 Comparing types} *)

(** Check if two types are compatible with each other. *)
let rec equate_tys (ty1 : ty) (ty2 : ty) : bool =
  match ty1, ty2 with
  | Unknown_type, _ | _, Unknown_type -> true
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      equate_tys param_ty1 param_ty2
      && equate_tys body_ty1 body_ty2
  | Int_type, Int_type -> true
  | Bool_type, Bool_type -> true
  | _, _ -> false

(** The “meet” of two types picks the most specific of two compatible types,
    which is important for propagating as much type information as we can when
    inferring the type of conditionals. This was inspired by section 2.1.6 of
    “Total Type Error Localization and Recovery with Holes” by Zhao et. al. *)
let rec meet_tys (ty1 : ty) (ty2 : ty) : ty option =
  let ( let* ) = Option.bind in
  match ty1, ty2 with
  | Unknown_type, ty | ty, Unknown_type -> Some ty
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      let* param_ty = meet_tys param_ty1 param_ty2 in
      let* body_ty = meet_tys body_ty1 body_ty2 in
      Some (Fun_type (param_ty, body_ty))
  | Int_type, Int_type -> Some Int_type
  | Bool_type, Bool_type -> Some Bool_type
  | _, _ -> None


(** {1 Pretty printing} *)

let pp_ty : ty -> Format.formatter -> unit =
  let rec pp_ty ty =
    match ty with
    | Fun_type (param_ty, body_ty) ->
        Format.dprintf "%t -> %t"
          (pp_atomic_ty param_ty)
          (pp_ty body_ty)
    | ty ->
        pp_atomic_ty ty
  and pp_atomic_ty ty =
    match ty with
    | Int_type -> Format.dprintf "Int"
    | Bool_type -> Format.dprintf "Bool"
    | Unknown_type -> Format.dprintf "?"
    | Fun_type _ as ty -> Format.dprintf "@[(%t)@]" (pp_ty ty)
  in
  pp_ty

let pp_name_ann (name : name) (ty : ty) : Format.formatter -> unit =
  Format.dprintf "@[<2>@[%s :@]@ %t@]" name (pp_ty ty)

let pp_param (name : name) (ty : ty) : Format.formatter -> unit =
  Format.dprintf "@[<2>(@[%s :@]@ %t)@]" name (pp_ty ty)

let pp_tm : name env -> tm -> Format.formatter -> unit =
  let rec pp_tm names tm =
    match tm with
    | Let _ as tm ->
        let rec go names tm =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.dprintf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_tm names def)
                (go (name :: names) body)
          | tm -> Format.dprintf "@[%t@]" (pp_tm names tm)
        in
        Format.dprintf "@[<v>%t@]" (go names tm)
    | Fun_lit (name, param_ty, body) ->
        let rec go names tm =
          match tm with
          | Fun_lit (name, param_ty, body) ->
              Format.dprintf "@ @[fun@ %t@ =>@]%t"
                (pp_param name param_ty)
                (go (name :: names) body)
          | tm -> Format.dprintf "@]@ @[%t@]@]" (pp_tm names tm)
        in
        Format.dprintf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
          (pp_param name param_ty)
          (go (name :: names) body)
    | Bool_elim (head, tm1, tm2) ->
        Format.dprintf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_tm names head)
          (pp_app_tm names tm1)
          (pp_tm names tm2)
    | tm ->
        pp_app_tm names tm
  and pp_app_tm names tm =
    let rec go tm =
      match tm with
      | Fun_app (head, arg) -> Format.dprintf "%t@ %t" (go head) (pp_atomic_tm names arg)
      | tm -> pp_atomic_tm names tm
    in
    match tm with
    | Fun_app _ as tm -> Format.dprintf "@[<hv 2>%t@]" (go tm)
    | tm -> pp_atomic_tm names tm
  and pp_atomic_tm names tm =
    match tm with
    | Var index -> Format.dprintf "%s" (List.nth names index)
    | Prim prim -> Format.dprintf "#%s" (Prim.name prim)
    | Int_lit i -> Format.dprintf "%i" i
    | Bool_lit true -> Format.dprintf "true"
    | Bool_lit false -> Format.dprintf "false"
    | Reported_error -> Format.dprintf "#reported-error"
    | Let _ | Fun_lit _ | Fun_app _ | Bool_elim _ as tm ->
        Format.dprintf "@[(%t)@]" (pp_tm names tm)
  in
  pp_tm
