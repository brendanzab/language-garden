(** A tree based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type term =
  | Num of int
  | Neg of term
  | Add of term * term
  | Sub of term * term
  | Mul of term * term
  | Div of term * term


let num n = Num n
let neg x = Neg x
let add x y = Add (x, y)
let sub x y = Sub (x, y)
let mul x y = Mul (x, y)
let div x y = Div (x, y)


(** Pretty printing *)

let rec pp_term fmt term =
  pp_add_term fmt term
and pp_add_term fmt = function
  | Add (t1, t2) -> Format.fprintf fmt "%a@ +@ %a" pp_mul_term t1 pp_add_term t2
  | Sub (t1, t2) -> Format.fprintf fmt "%a@ -@ %a" pp_mul_term t1 pp_add_term t2
  | t -> pp_mul_term fmt t
and pp_mul_term fmt = function
  | Mul (t1, t2) -> Format.fprintf fmt "%a@ *@ %a" pp_atomic_term t1 pp_mul_term t2
  | Div (t1, t2) -> Format.fprintf fmt "%a@ /@ %a" pp_atomic_term t1 pp_mul_term t2
  | t -> pp_atomic_term fmt t
and pp_atomic_term fmt = function
  | Num n -> Format.fprintf fmt "%d" n
  | Neg t -> Format.fprintf fmt "-%a" pp_atomic_term t
  | t -> Format.fprintf fmt "@[<1>(%a)@]" pp_term t


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int

  let rec eval : term -> value =
    function
    | Num n -> n
    | Neg n -> -(eval n)
    | Add (n1, n2) -> eval n1 + eval n2
    | Sub (n1, n2) -> eval n1 - eval n2
    | Mul (n1, n2) -> eval n1 * eval n2
    | Div (n1, n2) -> eval n1 / eval n2

end
