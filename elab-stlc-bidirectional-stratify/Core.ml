(** {0 Core language} *)

(** {1 Names} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string


(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** {i De Bruijn index} that represents a variable occurance by the number of
    binders between the occurance and the binder it refers to. *)
type index = int

(** {i De Bruijn level} that represents a variable occurance by the number of
    binders from the top of the environment to the binder that the ocurrance
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
  | FunType of ty * ty
  | IntType
  | BoolType

(** Primitive operations *)
type prim = [
  | `Eq   (** [Int -> Int -> Bool] *)
  | `Add  (** [Int -> Int -> Int] *)
  | `Sub  (** [Int -> Int -> Int] *)
  | `Mul  (** [Int -> Int -> Int] *)
  | `Neg  (** [Int -> Int] *)
]

(** Term syntax *)
type expr =
  | Var of index
  | Let of name * ty * expr * expr
  | FunLit of name * ty * expr
  | FunApp of expr * expr
  | IntLit of int
  | BoolLit of bool
  | BoolElim of expr * expr * expr
  | PrimApp of prim * expr list


module Semantics = struct

  (** {1 Values} *)

  type vexpr =
    | Neu of nexpr
    | FunLit of name * ty * (vexpr -> vexpr)
    | IntLit of int
    | BoolLit of bool

  and nexpr =
    | Var of level
    | FunApp of nexpr * vexpr
    | BoolElim of nexpr * vexpr Lazy.t * vexpr Lazy.t
    | PrimApp of prim * vexpr list


  (** {1 Eliminators} *)

  let fun_app head arg =
    match head with
    | Neu nexpr -> Neu (FunApp (nexpr, arg))
    | FunLit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim head vexpr0 vexpr1 =
    match head with
    | Neu nexpr -> Neu (BoolElim (nexpr, vexpr0, vexpr1))
    | BoolLit true -> Lazy.force vexpr0
    | BoolLit false -> Lazy.force vexpr1
    | _ -> invalid_arg "expected boolean"

  let prim_app prim args =
    match prim, args with
    | `Eq, [IntLit t1; IntLit t2] -> BoolLit (t1 = t2)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | prim, args -> Neu (PrimApp (prim, args))


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vexpr env) (e : expr) : vexpr =
    match e with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | FunLit (name, param_ty, body) ->
        FunLit (name, param_ty, fun arg -> eval (arg :: env) body)
    | FunApp (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, e0, e1) ->
        let head = eval env head in
        let ve0 = Lazy.from_fun (fun () -> eval env e0) in
        let ve1 = Lazy.from_fun (fun () -> eval env e1) in
        bool_elim head ve0 ve1
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : int) (ve : vexpr) : expr =
    match ve with
    | Neu ne -> quote_neu size ne
    | FunLit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        FunLit (name, param_ty, body)
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b

  and quote_neu (size : int) (ne : nexpr) : expr =
    match ne with
    | Var level ->
        Var (level_to_index size level)
    | FunApp (head, arg) ->
        FunApp (quote_neu size head, quote size arg)
    | BoolElim (head, vexpr0, vexpr1) ->
        let e0 = quote size (Lazy.force vexpr0) in
        let e1 = quote size (Lazy.force vexpr1) in
        BoolElim (quote_neu size head, e0, e1)
    | PrimApp (prim, args) ->
        PrimApp (prim, List.map (quote size) args)

  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vexpr list) (e : expr) : expr =
    quote (List.length env) (eval env e)

end


(** {1 Pretty printing} *)

let rec pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | FunType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt ty =
  match ty with
  | IntType -> Format.fprintf fmt "Int"
  | BoolType -> Format.fprintf fmt "Bool"
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
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[<2>@[fun@ %a@ =>@]@ %a@]"
        pp_param (name, param_ty)
        (pp_expr (name :: names)) body
  | e -> pp_if_expr names fmt e
and pp_if_expr names fmt e =
  match e with
  | BoolElim (head, e0, e1) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        (pp_eq_expr names) head
        (pp_eq_expr names) e0
        (pp_if_expr names) e1
  | e ->
      pp_eq_expr names fmt e
and pp_eq_expr names fmt e =
  match e with
  | PrimApp (`Eq, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ =@ %a@]"
        (pp_add_expr names) arg1
        (pp_eq_expr names) arg2
  | e ->
      pp_add_expr names fmt e
and pp_add_expr names fmt e =
  match e with
  | PrimApp (`Add, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ +@ %a@]"
        (pp_mul_expr names) arg1
        (pp_add_expr names) arg2
  | PrimApp (`Sub, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ -@ %a@]"
        (pp_mul_expr names) arg1
        (pp_add_expr names) arg2
  | e ->
      pp_mul_expr names fmt e
and pp_mul_expr names fmt e =
  match e with
  | PrimApp (`Mul, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ *@ %a@]"
        (pp_app_expr names) arg1
        (pp_mul_expr names) arg2
  | e ->
      pp_app_expr names fmt e
and pp_app_expr names fmt e =
  match e with
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_expr names) head
        (pp_atomic_expr names) arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        (pp_atomic_expr names) arg
  | e ->
      pp_atomic_expr names fmt e
and pp_atomic_expr names fmt e =
  match e with
  | Var index -> Format.fprintf fmt "%s" (List.nth names index)
  | IntLit i -> Format.fprintf fmt "%i" i
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  (* FIXME: Will loop forever on invalid primitive applications *)
  | e -> Format.fprintf fmt "@[(%a)@]" (pp_expr names) e
