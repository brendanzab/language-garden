(** A tree based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type expr =
  | Num of int
  | Bool of bool
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | IfThenElse of expr * expr * expr


let num n = Num n
let bool n = Bool n
let neg x = Neg x
let add x y = Add (x, y)
let sub x y = Sub (x, y)
let mul x y = Mul (x, y)
let div x y = Div (x, y)
let eq x y = Eq (x, y)
let if_then_else t1 t2 t3 = IfThenElse (t1, t2, t3)


(** Pretty printing *)

let rec pp_expr fmt expr =
  pp_if_expr fmt expr
and pp_if_expr fmt = function
  | IfThenElse (t1, t2, t3) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        pp_eq_expr t1
        pp_eq_expr t2
        pp_if_expr t3
  | t -> pp_eq_expr fmt t
and pp_eq_expr fmt = function
  | Eq (t1, t2) -> Format.fprintf fmt "%a@ =@ %a" pp_add_expr t1 pp_eq_expr t2
  | t -> pp_add_expr fmt t
and pp_add_expr fmt = function
  | Add (t1, t2) -> Format.fprintf fmt "%a@ +@ %a" pp_mul_expr t1 pp_add_expr t2
  | Sub (t1, t2) -> Format.fprintf fmt "%a@ -@ %a" pp_mul_expr t1 pp_add_expr t2
  | t -> pp_mul_expr fmt t
and pp_mul_expr fmt = function
  | Mul (t1, t2) -> Format.fprintf fmt "%a@ *@ %a" pp_atomic_expr t1 pp_mul_expr t2
  | Div (t1, t2) -> Format.fprintf fmt "%a@ /@ %a" pp_atomic_expr t1 pp_mul_expr t2
  | t -> pp_atomic_expr fmt t
and pp_atomic_expr fmt = function
  | Num n -> Format.fprintf fmt "%d" n
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"
  | Neg t -> Format.fprintf fmt "-%a" pp_atomic_expr t
  | t -> Format.fprintf fmt "@[<1>(%a)@]" pp_expr t


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Num of int
    | Bool of bool

  let rec eval : expr -> value =
    function
    | Num n -> Num n
    | Bool n -> Bool n
    | Neg n -> Num (-(eval_num n))
    | Add (n1, n2) -> Num (eval_num n1 + eval_num n2)
    | Sub (n1, n2) -> Num (eval_num n1 - eval_num n2)
    | Mul (n1, n2) -> Num (eval_num n1 * eval_num n2)
    | Div (n1, n2) -> Num (eval_num n1 / eval_num n2)
    | Eq (n1, n2) -> Bool (eval n1 = eval n2)
    | IfThenElse (n1, n2, n3) -> if eval_bool n1 then eval n2 else eval n3
  and eval_num expr =
    match eval expr with
    | Num n ->  n
    | _ -> failwith "not a number"
  and eval_bool expr =
    match eval expr with
    | Bool b ->  b
    | _ -> failwith "not a boolean"


  let quote : value -> expr =
    function
    | Num i -> Num i
    | Bool i -> Bool i


  let normalise t =
    quote (eval t)

end
