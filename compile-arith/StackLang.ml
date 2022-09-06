(** A stack based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type instruction =
  | Num of int    (** [       -- n     ] *)
  | Neg           (** [ n     -- -n    ] *)
  | Add           (** [ n1 n2 -- n1+n2 ] *)
  | Sub           (** [ n1 n2 -- n1-n2 ] *)
  | Mul           (** [ n1 n2 -- n1*n2 ] *)
  | Div           (** [ n1 n2 -- n1/n2 ] *)

type program =
  instruction list


(** Pretty printing *)

let pp_instruction fmt = function
  | Num n -> Format.fprintf fmt "num %d" n
  | Neg -> Format.fprintf fmt "neg"
  | Add -> Format.fprintf fmt "add"
  | Sub -> Format.fprintf fmt "sub"
  | Mul -> Format.fprintf fmt "mul"
  | Div -> Format.fprintf fmt "div"

let rec pp_program fmt = function
  | instruction :: program ->
      Format.fprintf fmt "%a@;%a"
        pp_instruction instruction
        pp_program program
  | [] -> ()


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int
  type stack = value list

  let step : (program * stack) -> (program * stack) = function
    | Num n :: program, stack -> program, n :: stack
    | Neg :: program, n :: stack -> program, -n :: stack
    | Add :: program, n2 :: n1 :: stack -> program, n1 + n2 :: stack
    | Sub :: program, n2 :: n1 :: stack -> program, n1 - n2 :: stack
    | Mul :: program, n2 :: n1 :: stack -> program, n1 * n2 :: stack
    | Div :: program, n2 :: n1 :: stack -> program, n1 / n2 :: stack
    | _, _ -> failwith "invalid program"

  let rec eval ?(stack = []) = function
    | [] -> stack
    | program ->
        let (program, stack) = step (program, stack) in
        eval program ~stack

end
