(** A tree based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type expr =
  | Int of int
  | Bool of bool
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | IfThenElse of expr * expr * expr


let int n = Int n
let bool b = Bool b
let neg e = Neg e
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)
let eq e1 e2 = Eq (e1, e2)
let if_then_else e1 e2 e3 = IfThenElse (e1, e2, e3)


(** Pretty printing *)

let rec pp_expr fmt expr =
  pp_if_expr fmt expr
and pp_if_expr fmt = function
  | IfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        pp_eq_expr e1
        pp_eq_expr e2
        pp_if_expr e3
  | e -> pp_eq_expr fmt e
and pp_eq_expr fmt = function
  | Eq (e1, e2) -> Format.fprintf fmt "%a@ =@ %a" pp_add_expr e1 pp_eq_expr e2
  | e -> pp_add_expr fmt e
and pp_add_expr fmt = function
  | Add (e1, e2) -> Format.fprintf fmt "%a@ +@ %a" pp_mul_expr e1 pp_add_expr e2
  | Sub (e1, e2) -> Format.fprintf fmt "%a@ -@ %a" pp_mul_expr e1 pp_add_expr e2
  | e -> pp_mul_expr fmt e
and pp_mul_expr fmt = function
  | Mul (e1, e2) -> Format.fprintf fmt "%a@ *@ %a" pp_atomic_expr e1 pp_mul_expr e2
  | Div (e1, e2) -> Format.fprintf fmt "%a@ /@ %a" pp_atomic_expr e1 pp_mul_expr e2
  | e -> pp_atomic_expr fmt e
and pp_atomic_expr fmt = function
  | Int n -> Format.fprintf fmt "%d" n
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"
  | Neg e -> Format.fprintf fmt "-%a" pp_atomic_expr e
  | e -> Format.fprintf fmt "@[<1>(%a)@]" pp_expr e


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Int of int
    | Bool of bool

  let rec eval : expr -> value =
    function
    | Int n -> Int n
    | Bool b -> Bool b
    | Neg e -> Int (-(eval_int e))
    | Add (e1, e2) -> Int (eval_int e1 + eval_int e2)
    | Sub (e1, e2) -> Int (eval_int e1 - eval_int e2)
    | Mul (e1, e2) -> Int (eval_int e1 * eval_int e2)
    | Div (e1, e2) -> Int (eval_int e1 / eval_int e2)
    | Eq (e1, e2) -> Bool (eval e1 = eval e2)
    | IfThenElse (e1, e2, e3) -> if eval_bool e1 then eval e2 else eval e3
  and eval_int expr =
    match eval expr with
    | Int n ->  n
    | _ -> failwith "not an integer"
  and eval_bool expr =
    match eval expr with
    | Bool b ->  b
    | _ -> failwith "not a boolean"


  let quote : value -> expr =
    function
    | Int i -> Int i
    | Bool b -> Bool b


  let normalise e =
    quote (eval e)

end
