(*
  Compilation from tree-based arithmetic expressions to stack-based instructions

  - https://blog.andrepopovitch.com/zinc/
  - https://www.marigold.dev/post/efficiently-implementing-the-lambda-calculus-with-zinc
*)

module TreeArith = struct

  type term =
    | Num of int
    | Add of term * term
    | Sub of term * term

  module Semantics = struct

    type value = int

    let rec eval : term -> value =
      function
      | Num n -> n
      | Add (n1, n2) -> eval n1 + eval n2
      | Sub (n1, n2) -> eval n1 - eval n2

  end

end

module StackArith = struct

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

end

(** A translation pass between two languages *)
module type Translation = sig
  (** The source language *)
  type source

  (** The target language *)
  type target

  (** The translation pass between the {!source} and {!target} languages *)
  val translate : source -> target
end

(** Converts between the {!TreeArith} language and the {!StackArith} language *)
module TreeToStack : Translation

  with type source := TreeArith.term
  with type target := StackArith.program

= struct

  let rec translate : TreeArith.term -> StackArith.program =
    function
    | TreeArith.Num n -> [StackArith.Num n]
    | TreeArith.Add (n1, n2) -> translate n1 @ translate n2 @ [StackArith.Add]
    | TreeArith.Sub (n1, n2) -> translate n1 @ translate n2 @ [StackArith.Sub]

end
