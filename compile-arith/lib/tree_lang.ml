(** {0 Arithmetic expressions}

    Arithmetic expressions as a tree of nested subexpressions.
*)


(** {1 Syntax of arithmetic expressions} *)

(** Expressions *)
type expr =
  | Int of int
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

(** {2 Constructor functions} *)

let int i = Int i
let neg e = Neg e
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)


(** {1 Pretty printing} *)

let rec pp_expr e ppf =
  pp_add_expr e ppf
and pp_add_expr e ppf =
  match e with
  | Add (e1, e2) -> Format.fprintf ppf "%t@ +@ %t" (pp_mul_expr e1) (pp_add_expr e2)
  | Sub (e1, e2) -> Format.fprintf ppf "%t@ -@ %t" (pp_mul_expr e1) (pp_add_expr e2)
  | e -> pp_mul_expr e ppf
and pp_mul_expr e ppf =
  match e with
  | Mul (e1, e2) -> Format.fprintf ppf "%t@ *@ %t" (pp_atomic_expr e1) (pp_mul_expr e2)
  | Div (e1, e2) -> Format.fprintf ppf "%t@ /@ %t" (pp_atomic_expr e1) (pp_mul_expr e2)
  | e -> pp_atomic_expr e ppf
and pp_atomic_expr e ppf =
  match e with
  | Int i -> Format.fprintf ppf "%d" i
  | Neg e -> Format.fprintf ppf "-%t" (pp_atomic_expr e)
  | Add _ | Sub _ | Mul _ | Div _ as e ->
      Format.fprintf ppf "@[<1>(%t)@]" (pp_expr e)


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int

  let rec eval : expr -> value =
    function
    | Int i -> i
    | Neg e -> -(eval e)
    | Add (e1, e2) -> eval e1 + eval e2
    | Sub (e1, e2) -> eval e1 - eval e2
    | Mul (e1, e2) -> eval e1 * eval e2
    | Div (e1, e2) -> eval e1 / eval e2

end
