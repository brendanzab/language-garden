(** A core functional language made up of the simply typed lambda calculus
    extended with let bindings, integers booleans, and primitive operations. *)

type name = Name.t
type index = int

type ty =
  | BoolTy                                        (* Bool *)
  | IntTy                                         (* Int *)
  | FunTy of ty * ty                              (* t1 -> t2 *)
  | TupleTy of ty list                            (* (t1, ... tn) *)

type expr =
  | Var of index                                  (* x *)
  | Prim of Prim.t                                (* #p *)
  | Let of name * ty * expr * expr                (* let x : t := e1; e2 *)
  | FunLit of name * ty * expr                    (* fun (x : t) => e *)
  | FunApp of expr * expr                         (* e1 e2 *)
  | TupleLit of expr list                         (* (e1, ..., en) *)
  | TupleProj of expr * int                       (* e.n *)
  | BoolLit of bool                               (* true | false *)
  | BoolElim of expr * expr * expr                (* if e1 then e2 else e3 *)
  | IntLit of int                                 (* ... | -1 | 0 | 1 | ... *)

(** {1 Pretty printing} *)

let pp_comma_sep (fmt : Format.formatter) () =
  Format.fprintf fmt ",@ "

let pp_tuple_elems (type a) (pp_elem : Format.formatter -> a -> unit) (fmt : Format.formatter) (elems : a list) =
  match elems with
  | [elem] -> Format.fprintf fmt "%a," pp_elem elem
  | elems ->
      Format.fprintf fmt "%a"
        (Format.pp_print_list pp_elem ~pp_sep:pp_comma_sep) elems

let rec pp_ty (fmt : Format.formatter) (ty : ty) =
  match ty with
  | FunTy (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty (fmt : Format.formatter) (ty : ty) =
  match ty with
  | TupleTy tys -> Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_ty) tys
  | BoolTy -> Format.fprintf fmt "Bool"
  | IntTy -> Format.fprintf fmt "Int"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_name_ann (fmt : Format.formatter) (name, ty) =
  Format.fprintf fmt "@[<2>@[%a :@]@ %a@]" Name.pp name pp_ty ty

let pp_param (fmt : Format.formatter) (name, ty) =
  Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]" Name.pp name pp_ty ty

let rec pp_expr (names : Name.t list) (fmt : Format.formatter) (expr : expr) =
  match expr with
  | Let _ as expr ->
      let rec go (names : Name.t list) (fmt : Format.formatter) (expr : expr) =
        match expr with
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_expr names) def
              (go (name :: names)) body
        | expr -> Format.fprintf fmt "@[%a@]" (pp_expr names) expr
      in
      go names fmt expr
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[@[fun@ %a@ =>@]@ %a@]"
        pp_param (name, param_ty)
        (pp_expr (name :: names)) body
  | expr ->
      pp_if_expr names fmt expr
and pp_if_expr (names : Name.t list) (fmt : Format.formatter) (expr : expr) =
  match expr with
  | BoolElim (head, on_true, on_false) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        (pp_app_expr names) head
        (pp_app_expr names) on_true
        (pp_if_expr names) on_false
  | expr ->
      pp_app_expr names fmt expr
and pp_app_expr (names : Name.t list) (fmt : Format.formatter) (expr : expr) =
  match expr with
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_expr names) head
        (pp_proj_expr names) arg
  | expr ->
      pp_proj_expr names fmt expr
and pp_proj_expr (names : Name.t list) (fmt : Format.formatter) (expr : expr) =
  match expr with
  | TupleProj (head, label) ->
      Format.fprintf fmt "@[%a.%i@]"
        (pp_proj_expr names) head
        label
  | tm ->
      pp_atomic_expr names fmt tm
and pp_atomic_expr (names : Name.t list) (fmt : Format.formatter) (expr : expr) =
  match expr with
  | Var index -> Name.pp fmt (List.nth names index)
  | Prim prim -> Format.fprintf fmt "#%s" (Prim.name prim)
  | TupleLit exprs -> Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems (pp_expr names)) exprs
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  | expr -> Format.fprintf fmt "@[(%a)@]" (pp_expr names) expr

module Semantics = struct

  type vexpr =
    | Prim of Prim.t
    | IntLit of int
    | BoolLit of bool
    | FunLit of (vexpr -> vexpr)
    | TupleLit of vexpr list

  type env = vexpr list

  let prim_of_vexpr (vexpr : vexpr) : Prim.value =
    match vexpr with
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | _ -> failwith "invalid primitive"

  let vexpr_of_prim (v : Prim.value) : vexpr =
    match v with
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i

  let prim_app (prim : Prim.t) (arg : vexpr) : vexpr =
    match arg with
    | TupleLit args -> vexpr_of_prim (Prim.app prim (List.map prim_of_vexpr args))
    | _ -> invalid_arg "invalid prim app"

  let rec eval (env : env) (expr : expr) : vexpr =
    match expr with
    | Var index -> List.nth env index
    | Prim prim -> Prim prim
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | FunLit (_, _, body) ->
        FunLit (fun arg -> eval (arg :: env) body)
    | FunApp (head, arg) ->
        begin match eval env head with
        | Prim prim -> prim_app prim (eval env arg)
        | FunLit body -> body (eval env arg)
        | _ -> invalid_arg "expected function"
        end
    | TupleLit exprs ->
        TupleLit (List.map (eval env) exprs)
    | TupleProj (head, label) ->
        begin match eval env head with
        | TupleLit evs -> List.nth evs label
        | _ -> invalid_arg "expected tuple"
        end
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, on_true, on_false) ->
        begin match eval env head with
        | BoolLit true -> eval env on_true
        | BoolLit false -> eval env on_false
        | _ -> invalid_arg "expected boolean"
        end

end

(** Get the type of an expression, assuming that it is well-typed. *)
let rec type_of (local_tys : ty list) (expr : expr) : ty =
  match expr with
  | Var index -> List.nth local_tys index
  | Prim prim ->
      let param_tys, body_ty = Prim.ty prim in
      FunTy (TupleTy (List.map type_of_prim param_tys), type_of_prim body_ty)
  | Let (_, def_ty, _, body) -> type_of (def_ty :: local_tys) body
  | FunLit (_, param_ty, body) ->
      FunTy (param_ty, type_of (param_ty :: local_tys) body)
  | FunApp (head, _) ->
      begin match type_of local_tys head with
      | FunTy (_, body_ty) -> body_ty
      | _ -> invalid_arg "expected function type"
      end
  | TupleLit exprs -> TupleTy (List.map (type_of local_tys) exprs)
  | TupleProj (head, label) ->
      begin match type_of local_tys head with
      | TupleTy tys -> List.nth tys label
      | _ -> invalid_arg "expected tuple type"
      end
  | BoolLit _ -> BoolTy
  | BoolElim (_, on_true, _) -> type_of local_tys on_true
  | IntLit _ -> IntTy
and type_of_prim (prim_ty : Prim.ty) : ty =
  match prim_ty with
  | BoolTy -> BoolTy
  | IntTy -> IntTy
