(** A stack based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type inst =
  | Num of int    (** [       -- n     ] *)
  | Neg           (** [ n     -- -n    ] *)
  | Add           (** [ n1 n2 -- n1+n2 ] *)
  | Sub           (** [ n1 n2 -- n1-n2 ] *)
  | Mul           (** [ n1 n2 -- n1*n2 ] *)
  | Div           (** [ n1 n2 -- n1/n2 ] *)

type code =
  inst list


(** Pretty printing *)

let pp_inst fmt = function
  | Num n -> Format.fprintf fmt "%d" n
  | Neg -> Format.fprintf fmt "neg"
  | Add -> Format.fprintf fmt "add"
  | Sub -> Format.fprintf fmt "sub"
  | Mul -> Format.fprintf fmt "mul"
  | Div -> Format.fprintf fmt "div"
let rec pp_code fmt = function
  | [] -> ()
  | inst :: [] -> Format.fprintf fmt "%a" pp_inst inst
  | inst :: code -> Format.fprintf fmt "%a@ %a" pp_inst inst pp_code code


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int
  type stack = value list

  let step : (code * stack) -> (code * stack) = function
    | Num n :: code, stack -> code, n :: stack
    | Neg :: code, n :: stack -> code, -n :: stack
    | Add :: code, n2 :: n1 :: stack -> code, n1 + n2 :: stack
    | Sub :: code, n2 :: n1 :: stack -> code, n1 - n2 :: stack
    | Mul :: code, n2 :: n1 :: stack -> code, n1 * n2 :: stack
    | Div :: code, n2 :: n1 :: stack -> code, n1 / n2 :: stack
    | _, _ -> failwith "invalid code"

  let rec eval ?(stack = []) = function
    | [] -> stack
    | code ->
        let (code, stack) = step (code, stack) in
        eval code ~stack

end
