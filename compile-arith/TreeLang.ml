(** A tree based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type expr =
  | Num of int
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr


let num n = Num n
let neg x = Neg x
let add x y = Add (x, y)
let sub x y = Sub (x, y)
let mul x y = Mul (x, y)
let div x y = Div (x, y)


(** Pretty printing *)

let rec pp_expr fmt expr =
  pp_add_expr fmt expr
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
  | Neg t -> Format.fprintf fmt "-%a" pp_atomic_expr t
  | t -> Format.fprintf fmt "@[<1>(%a)@]" pp_expr t


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int

  let rec eval : expr -> value =
    function
    | Num n -> n
    | Neg n -> -(eval n)
    | Add (n1, n2) -> eval n1 + eval n2
    | Sub (n1, n2) -> eval n1 - eval n2
    | Mul (n1, n2) -> eval n1 * eval n2
    | Div (n1, n2) -> eval n1 / eval n2

end
