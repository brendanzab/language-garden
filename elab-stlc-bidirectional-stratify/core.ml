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

(** Term syntax *)
type expr =
  | Var of index
  | Prim of Prim.t
  | Let of name * ty * expr * expr
  | Fun_lit of name * ty * expr
  | Fun_app of expr * expr
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of expr * expr * expr

(** Apply an expression to a list of arguments *)
let fun_app (head : expr) (args : expr list) : expr =
  List.fold_left (fun head arg -> Fun_app (head, arg)) head args


module Semantics = struct

  (** {1 Values} *)

  (** Expressions in weak head normal form (i.e. values) *)
  type vexpr =
    | Neu of nexpr
    | Fun_lit of name * ty * clos1
    | Int_lit of int
    | Bool_lit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} expressions back to syntax, which is useful
      for pretty printing under binders.
  *)
  and nexpr =
    | Var of level
    | Prim_app of Prim.t * vexpr list
    | Fun_app of nexpr * vexpr
    | Bool_elim of nexpr * clos0 * clos0

  (** Delayed computations that can be forced later. *)
  and clos0 = Clos0 of vexpr env * expr

  (** Closures that can be instantiated with a value. The environment provides
      a value for each variable in the term, except for the variable that the
      closure will be instantiated with during evaluation. *)
  and clos1 = Clos1 of vexpr env * expr


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vexpr env) (e : expr) : vexpr =
    match e with
    | Var index -> List.nth env index
    | Prim prim -> prim_app prim []
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (name, param_ty, body) ->
        Fun_lit (name, param_ty, Clos1 (env, body))
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, e1, e2) ->
        let head = eval env head in
        bool_elim head (Clos0 (env, e1)) (Clos0 (env, e2))

  (** {2 Instantiation} *)

  and inst0 (Clos0 (env, body) : clos0) : vexpr =
    eval env body

  and inst1 (Clos1 (env, body) : clos1) (arg : vexpr) : vexpr =
    eval (arg :: env) body

  (** {2 Eliminators} *)

  and prim_app (prim : Prim.t) (args : vexpr list) : vexpr =
    match prim, args with
    | Bool_eq, [Bool_lit t2; Bool_lit t1] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq, [Int_lit t2; Int_lit t1] -> Bool_lit (Int.equal t1 t2)
    | Int_add, [Int_lit t2; Int_lit t1] -> Int_lit (Int.add t1 t2)
    | Int_sub, [Int_lit t2; Int_lit t1] -> Int_lit (Int.sub t1 t2)
    | Int_mul, [Int_lit t2; Int_lit t1] -> Int_lit (Int.mul t1 t2)
    | Int_neg, [Int_lit t1] -> Int_lit (Int.neg t1)
    | prim, args -> Neu (Prim_app (prim, args))

  and fun_app (head : vexpr) (arg : vexpr) : vexpr =
    match head with
    | Neu (Prim_app (prim, args)) -> prim_app prim (arg :: args)
    | Neu nexpr -> Neu (Fun_app (nexpr, arg))
    | Fun_lit (_, _, body) -> inst1 body arg
    | _ -> invalid_arg "expected function"

  and bool_elim (head : vexpr) (vexpr1 : clos0) (vexpr2 : clos0) : vexpr =
    match head with
    | Neu nexpr -> Neu (Bool_elim (nexpr, vexpr1, vexpr2))
    | Bool_lit true -> inst0 vexpr1
    | Bool_lit false -> inst0 vexpr2
    | _ -> invalid_arg "expected boolean"


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (ve : vexpr) : expr =
    match ve with
    | Neu ne -> quote_neu size ne
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (inst1 body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : level) (ne : nexpr) : expr =
    match ne with
    | Var level ->
        Var (level_to_index size level)
    | Prim_app (prim, []) -> Prim prim
    | Prim_app (prim, arg :: args) ->
        Fun_app (quote_neu size (Prim_app (prim, args)), quote size arg)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Bool_elim (head, vexpr1, vexpr2) ->
        let e1 = quote size (inst0 vexpr1) in
        let e2 = quote size (inst0 vexpr2) in
        Bool_elim (quote_neu size head, e1, e2)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vexpr list) (e : expr) : expr =
    quote (List.length env) (eval env e)

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
    | Fun_type _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
  in
  pp_ty

let pp_name_ann (name : string) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%s :@]@ %t@]" name (pp_ty ty)

let pp_param (name : string) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%s :@]@ %t)@]" name (pp_ty ty)

let pp_expr : name env -> expr -> Format.formatter -> unit =
  let rec pp_expr names e ppf =
    match e with
    | Let _ as e ->
        let rec go names e ppf =
          match e with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_expr names def)
                (go (name :: names) body)
          | e -> Format.fprintf ppf "@[%t@]" (pp_expr names e)
        in
        Format.fprintf ppf "@[<v>%t@]" (go names e)
    | Fun_lit (name, param_ty, body) ->
        let rec go names e ppf =
          match e with
          | Fun_lit (name, param_ty, body) ->
              Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
                (pp_param name param_ty)
                (go (name :: names) body)
          | e -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_expr names e)
        in
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
          (pp_param name param_ty)
          (go (name :: names) body)
    | Bool_elim (head, e1, e2) ->
        Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_expr names head)
          (pp_app_expr names e1)
          (pp_expr names e2)
    | e ->
        pp_app_expr names e ppf
  and pp_app_expr names e ppf =
    let rec go e ppf =
      match e with
      | Fun_app (head, arg) ->
          Format.fprintf ppf "%t@ %t"
            (go head)
            (pp_atomic_expr names arg)
      | e ->
          pp_atomic_expr names e ppf
    in
    match e with
    | Fun_app _ as e ->
        Format.fprintf ppf "@[<hv 2>%t@]" (go e)
    | e ->
        pp_atomic_expr names e ppf
  and pp_atomic_expr names e ppf =
    match e with
    | Var index -> Format.fprintf ppf "%s" (List.nth names index)
    | Prim prim -> Format.fprintf ppf "#%s" (Prim.name prim)
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Let _ | Fun_lit _ | Fun_app _ | Bool_elim _ as e ->
        Format.fprintf ppf "@[(%t)@]" (pp_expr names e)
  in
  pp_expr
