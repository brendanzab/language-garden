(** A core functional language made up of the simply typed lambda calculus
    extended with let bindings, integers booleans, and primitive operations. *)

type name = Name.t
type index = int

type ty =
  | Bool_ty                                       (* Bool *)
  | Int_ty                                        (* Int *)
  | Fun_ty of ty * ty                             (* t1 -> t2 *)
  | Tuple_ty of ty list                           (* (t1, ... tn) *)

type expr =
  | Var of index                                  (* x *)
  | Prim of Prim.t                                (* #p *)
  | Let of name * ty * expr * expr                (* let x : t := e1; e2 *)
  | Fun_lit of name * ty * expr                   (* fun (x : t) => e *)
  | Fun_app of expr * expr                        (* e1 e2 *)
  | Tuple_lit of expr list                        (* (e1, ..., en) *)
  | Tuple_proj of expr * int                      (* e.n *)
  | Bool_lit of bool                              (* true | false *)
  | Bool_elim of expr * expr * expr               (* if e1 then e2 else e3 *)
  | Int_lit of int                                (* ... | -1 | 0 | 1 | ... *)

(** {1 Pretty printing} *)

let pp_comma_sep (ppf : Format.formatter) () =
  Format.fprintf ppf ",@ "

let pp_tuple_elems (type a) (pp_elem : Format.formatter -> a -> unit) (ppf : Format.formatter) (elems : a list) =
  match elems with
  | [elem] -> Format.fprintf ppf "%a," pp_elem elem
  | elems ->
      Format.fprintf ppf "%a"
        (Format.pp_print_list pp_elem ~pp_sep:pp_comma_sep) elems

let rec pp_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Fun_ty (param_ty, body_ty) ->
      Format.fprintf ppf "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty ppf ty
and pp_atomic_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Tuple_ty tys -> Format.fprintf ppf "@[(%a)@]" (pp_tuple_elems pp_ty) tys
  | Bool_ty -> Format.fprintf ppf "Bool"
  | Int_ty -> Format.fprintf ppf "Int"
  | ty -> Format.fprintf ppf "@[(%a)@]" pp_ty ty

let pp_name_ann (ppf : Format.formatter) (name, ty) =
  Format.fprintf ppf "@[<2>@[%a :@]@ %a@]" Name.pp name pp_ty ty

let pp_param (ppf : Format.formatter) (name, ty) =
  Format.fprintf ppf "@[<2>(@[%a :@]@ %a)@]" Name.pp name pp_ty ty

let rec pp_expr (names : Name.t list) (ppf : Format.formatter) (expr : expr) =
  match expr with
  | Let _ as expr ->
      let rec go (names : Name.t list) (ppf : Format.formatter) (expr : expr) =
        match expr with
        | Let (name, def_ty, def, body) ->
            Format.fprintf ppf "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_expr names) def
              (go (name :: names)) body
        | expr -> Format.fprintf ppf "@[%a@]" (pp_expr names) expr
      in
      go names ppf expr
  | Fun_lit (name, param_ty, body) ->
      Format.fprintf ppf "@[@[fun@ %a@ =>@]@ %a@]"
        pp_param (name, param_ty)
        (pp_expr (name :: names)) body
  | expr ->
      pp_if_expr names ppf expr
and pp_if_expr (names : Name.t list) (ppf : Format.formatter) (expr : expr) =
  match expr with
  | Bool_elim (head, on_true, on_false) ->
      Format.fprintf ppf "@[if@ %a@ then@]@ %a@ else@ %a"
        (pp_app_expr names) head
        (pp_app_expr names) on_true
        (pp_if_expr names) on_false
  | expr ->
      pp_app_expr names ppf expr
and pp_app_expr (names : Name.t list) (ppf : Format.formatter) (expr : expr) =
  match expr with
  | Fun_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        (pp_app_expr names) head
        (pp_proj_expr names) arg
  | expr ->
      pp_proj_expr names ppf expr
and pp_proj_expr (names : Name.t list) (ppf : Format.formatter) (expr : expr) =
  match expr with
  | Tuple_proj (head, label) ->
      Format.fprintf ppf "@[%a.%i@]"
        (pp_proj_expr names) head
        label
  | tm ->
      pp_atomic_expr names ppf tm
and pp_atomic_expr (names : Name.t list) (ppf : Format.formatter) (expr : expr) =
  match expr with
  | Var index -> Name.pp ppf (List.nth names index)
  | Prim prim -> Format.fprintf ppf "#%s" (Prim.name prim)
  | Tuple_lit exprs -> Format.fprintf ppf "@[(%a)@]" (pp_tuple_elems (pp_expr names)) exprs
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | Int_lit i -> Format.fprintf ppf "%i" i
  | expr -> Format.fprintf ppf "@[(%a)@]" (pp_expr names) expr

module Semantics = struct

  type vexpr =
    | Prim of Prim.t
    | Int_lit of int
    | Bool_lit of bool
    | Fun_lit of (vexpr -> vexpr)
    | Tuple_lit of vexpr list

  type env = vexpr list

  let prim_of_vexpr (vexpr : vexpr) : Prim.value =
    match vexpr with
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i
    | _ -> failwith "invalid primitive"

  let vexpr_of_prim (v : Prim.value) : vexpr =
    match v with
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i

  let prim_app (prim : Prim.t) (arg : vexpr) : vexpr =
    match arg with
    | Tuple_lit args -> vexpr_of_prim (Prim.app prim (List.map prim_of_vexpr args))
    | _ -> invalid_arg "invalid prim app"

  let rec eval (env : env) (expr : expr) : vexpr =
    match expr with
    | Var index -> List.nth env index
    | Prim prim -> Prim prim
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (_, _, body) ->
        Fun_lit (fun arg -> eval (arg :: env) body)
    | Fun_app (head, arg) ->
        begin match eval env head with
        | Prim prim -> prim_app prim (eval env arg)
        | Fun_lit body -> body (eval env arg)
        | _ -> invalid_arg "expected function"
        end
    | Tuple_lit exprs ->
        Tuple_lit (List.map (eval env) exprs)
    | Tuple_proj (head, label) ->
        begin match eval env head with
        | Tuple_lit evs -> List.nth evs label
        | _ -> invalid_arg "expected tuple"
        end
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, on_true, on_false) ->
        begin match eval env head with
        | Bool_lit true -> eval env on_true
        | Bool_lit false -> eval env on_false
        | _ -> invalid_arg "expected boolean"
        end

end

(** Get the type of an expression, assuming that it is well-typed. *)
let rec type_of (local_tys : ty list) (expr : expr) : ty =
  match expr with
  | Var index -> List.nth local_tys index
  | Prim prim ->
      let param_tys, body_ty = Prim.ty prim in
      Fun_ty (Tuple_ty (List.map type_of_prim param_tys), type_of_prim body_ty)
  | Let (_, def_ty, _, body) -> type_of (def_ty :: local_tys) body
  | Fun_lit (_, param_ty, body) ->
      Fun_ty (param_ty, type_of (param_ty :: local_tys) body)
  | Fun_app (head, _) ->
      begin match type_of local_tys head with
      | Fun_ty (_, body_ty) -> body_ty
      | _ -> invalid_arg "expected function type"
      end
  | Tuple_lit exprs -> Tuple_ty (List.map (type_of local_tys) exprs)
  | Tuple_proj (head, label) ->
      begin match type_of local_tys head with
      | Tuple_ty tys -> List.nth tys label
      | _ -> invalid_arg "expected tuple type"
      end
  | Bool_lit _ -> Bool_ty
  | Bool_elim (_, on_true, _) -> type_of local_tys on_true
  | Int_lit _ -> Int_ty
and type_of_prim (prim_ty : Prim.ty) : ty =
  match prim_ty with
  | Bool_ty -> Bool_ty
  | Int_ty -> Int_ty
