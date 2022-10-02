(** {0 A stack machine for arithmetic expressions} *)

(** This represents arithmetic expressions as a
    {{:https://en.wikipedia.org/wiki/Stack_machine} stack machine}. *)


(** Syntax of arithmetic expressions *)

type inst =
  | Int of int    (** [       -- i     ] *)
  | Neg           (** [ i     -- -n    ] *)
  | Add           (** [ i1 i2 -- i1+i2 ] *)
  | Sub           (** [ i1 i2 -- i1-i2 ] *)
  | Mul           (** [ i1 i2 -- i1*i2 ] *)
  | Div           (** [ i1 i2 -- i1/i2 ] *)

type code =
  inst list


(** Pretty printing *)

let pp_inst fmt = function
  | Int i -> Format.fprintf fmt "int %d" i
  | Neg -> Format.fprintf fmt "neg"
  | Add -> Format.fprintf fmt "add"
  | Sub -> Format.fprintf fmt "sub"
  | Mul -> Format.fprintf fmt "mul"
  | Div -> Format.fprintf fmt "div"
let rec pp_code fmt = function
  | [] -> ()
  | inst :: [] -> Format.fprintf fmt "%a;" pp_inst inst
  | inst :: code -> Format.fprintf fmt "%a;@ %a" pp_inst inst pp_code code


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int
  type stack = value list

  let step : (code * stack) -> (code * stack) = function
    | Int i :: code, stack -> code, i :: stack
    | Neg :: code, i :: stack -> code, -i :: stack
    | Add :: code, i2 :: i1 :: stack -> code, i1 + i2 :: stack
    | Sub :: code, i2 :: i1 :: stack -> code, i1 - i2 :: stack
    | Mul :: code, i2 :: i1 :: stack -> code, i1 * i2 :: stack
    | Div :: code, i2 :: i1 :: stack -> code, i1 / i2 :: stack
    | _, _ -> failwith "invalid code"

  let rec eval ?(stack = []) = function
    | [] -> stack
    | code ->
        let (code, stack) = step (code, stack) in
        eval code ~stack

  let rec quote : stack -> code = function
    | [] -> []
    | i :: stack -> Int i :: quote stack

  let normalise t =
    quote (eval t)

end
