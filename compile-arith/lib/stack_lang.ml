(** {0 A stack machine for arithmetic expressions}

    This represents arithmetic expressions as a
    {{: https://en.wikipedia.org/wiki/Stack_machine} stack machine}.
*)


(** {1 Stack machine syntax} *)

(** Stack machine instructions *)
type inst =
  | Int of int    (** [       -- i     ] *)
  | Neg           (** [ i     -- -n    ] *)
  | Add           (** [ i1 i2 -- i1+i2 ] *)
  | Sub           (** [ i1 i2 -- i1-i2 ] *)
  | Mul           (** [ i1 i2 -- i1*i2 ] *)
  | Div           (** [ i1 i2 -- i1/i2 ] *)

(** Instruction sequences *)
type code =
  inst list


(** {1 Pretty printing} *)

let rec pp_inst inst ppf =
  match inst with
  | Int i -> Format.fprintf ppf "int %d" i
  | Neg -> Format.fprintf ppf "neg"
  | Add -> Format.fprintf ppf "add"
  | Sub -> Format.fprintf ppf "sub"
  | Mul -> Format.fprintf ppf "mul"
  | Div -> Format.fprintf ppf "div"
and pp_code code ppf =
  match code with
  | [] -> ()
  | inst :: [] -> Format.fprintf ppf "%t;" (pp_inst inst)
  | inst :: code -> Format.fprintf ppf "%t;@ %t" (pp_inst inst) (pp_code code)


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value = int

  (** A stack of values *)
  type stack = value list

  (* The state of the stack machine *)
  type state = code * stack

  let step : state -> state = function
    | Int i :: code, stack -> code, i :: stack
    | Neg :: code, i :: stack -> code, -i :: stack
    | Add :: code, i2 :: i1 :: stack -> code, i1 + i2 :: stack
    | Sub :: code, i2 :: i1 :: stack -> code, i1 - i2 :: stack
    | Mul :: code, i2 :: i1 :: stack -> code, i1 * i2 :: stack
    | Div :: code, i2 :: i1 :: stack -> code, i1 / i2 :: stack
    | _, _ -> failwith "invalid code"

  let rec eval : state -> stack = function
    | [], stack -> stack
    | code, stack -> (eval [@tailcall]) (step (code, stack))

  let [@tail_mod_cons] rec quote : stack -> code = function
    | [] -> []
    | i :: stack -> Int i :: quote stack

  let normalise t =
    quote (eval t)

end
