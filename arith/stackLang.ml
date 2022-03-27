type instruction =
  | Num of int
  | Add
  | Sub

type program =
  instruction list

module Semantics = struct

  type value = int
  type stack = value list

  type state = {
    program : program;
    stack : stack;
  }

  let step : state -> state =
    function
    | { program = Num n :: program; stack } -> { program = program; stack = n :: stack }
    | { program = Add :: program; stack = n1 :: n2 :: stack } -> { program = program; stack = n1 + n2 :: stack }
    | { program = Sub :: program; stack = n1 :: n2 :: stack } -> { program = program; stack = n1 - n2 :: stack }
    | { program = _; stack = _ } -> failwith "invalid program"

end
