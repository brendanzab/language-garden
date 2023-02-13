(** {0 Core language} *)

(** {1 Syntax} *)

type prim = [
  | `Neg
  | `Add
  | `Sub
  | `Mul
]

type ty =
  | BoolType
  | IntType
  | FunType of ty * ty

type tm =
  | Var of int
  | Let of string * ty * tm * tm
  | BoolLit of bool
  | IntLit of int
  | PrimApp of prim * tm list
  | FunLit of string * ty * tm
  | FunApp of tm * tm


(** {1 Pretty printing} *)

let rec pp_ty fmt =
  function
  | FunType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt =
  function
  | BoolType -> Format.fprintf fmt "Bool"
  | IntType -> Format.fprintf fmt "Int"
  | ty ->
      Format.fprintf fmt "@[(%a)@]" pp_ty ty


let rec pp_tm names fmt = function
  | Let (name, def_ty, def, body) ->
      Format.fprintf fmt "@[<2>@[let@ %s@ :@ %a@ :=@]@ %a;@]@ %a"
        name
        pp_ty def_ty
        (pp_tm names) def
        (pp_tm (name :: names)) body
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[@[fun@ @[(%s@ :@ %a)@]@ =>@]@ %a@]"
        name
        pp_ty param_ty
        (pp_tm (name :: names)) body
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt = function
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | PrimApp (prim, args) ->
      Format.fprintf fmt "@[<2>%s@ %a@]"
        (match prim with
          | `Neg -> "#neg"
          | `Add -> "#add"
          | `Sub -> "#sub"
          | `Mul -> "#mul")
        (Format.pp_print_list (pp_atomic_tm names) ~pp_sep:Format.pp_print_space) args
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt = function
  | Var index ->
      Format.fprintf fmt "%s" (List.nth names index)
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm


module Build = struct

  type 'a m = int -> 'a

  let var (level : int) : tm m =
    fun size ->
      Var (size - level - 1)

  let bind_var (cont : int -> 'a m) : 'a m =
    fun size ->
      cont size (size + 1)

  let scope (cont : tm m -> 'a m) : 'a m =
    bind_var (fun level -> cont (var level))


  (** {1 Types} *)

  let bool_ty : ty = BoolType

  let int_ty : ty = IntType

  let fun_ty (param_tys : ty list) (body_ty : ty) : ty =
    List.fold_right
      (fun param_ty acc -> FunType (param_ty, acc))
      param_tys
      body_ty


  (** {1 Terms} *)

  let let_ (name : string) (def_ty : ty) (def : tm m) (body : tm m -> tm m) : tm m =
    fun env ->
      Let (name, def_ty, def env, scope body env)

  let ( let* ) (name, ty, tm) body =
    let_ name ty tm body

  let bool_lit (b : bool) : tm m =
    fun _ -> BoolLit b

  let int_lit (i : int) : tm m =
    fun _ -> IntLit i

  let prim_app (prim : prim) (args : tm m list) : tm m =
    fun env ->
      PrimApp (prim, List.map (fun arg -> arg env) args)

  let neg x : tm m =
    prim_app `Neg [x]

  let ( + ) x y : tm m =
    prim_app `Add [x; y]

  let ( - ) x y : tm m =
    prim_app `Sub [x; y]

  let ( * ) x y : tm m =
    prim_app `Mul [x; y]

  let fun_lit (name : string) (param_ty : ty) (body : tm m -> tm m) : tm m =
    fun env ->
      FunLit (name, param_ty, scope body env)

  let fun_app (head : tm m) (arg : tm m) : tm m =
    fun env ->
      FunApp (head env, arg env)

  let fun_apps (head : tm m) (args : tm m list) : tm m =
    List.fold_left fun_app head args

end


module Semantics = struct

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | FunLit of string * ty * clos

  and clos =
    vtm list * tm


  let rec eval env : tm -> vtm =
    function
    | Var x -> List.nth env x
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | PrimApp (`Neg, [IntLit t1]) -> IntLit (-t1)
    | PrimApp (`Add, [IntLit t1; IntLit t2]) -> IntLit (t1 + t2)
    | PrimApp (`Sub, [IntLit t1; IntLit t2]) -> IntLit (t1 - t2)
    | PrimApp (`Mul, [IntLit t1; IntLit t2]) -> IntLit (t1 * t2)
    | PrimApp _ -> invalid_arg "invalid prim application"
    | FunLit (name, param_ty, body) ->
        FunLit (name, param_ty, (env, body))
    | FunApp (head, arg) ->
        begin match eval env head with
        | FunLit (_, _, (env, body)) ->
            let arg = eval env arg in
            eval (arg :: env) body
        | _ -> invalid_arg "function expected"
        end

end
