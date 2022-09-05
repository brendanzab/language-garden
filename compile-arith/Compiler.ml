module TreeLang = struct

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


module StackLang = struct
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


(** Translation pass between the {!TreeLang} and {!StackLang} *)
module TreeToStack : Translation

  with type source = TreeLang.term
  with type target = StackLang.program

= struct

  type source = TreeLang.term
  type target = StackLang.program

  let rec translate : TreeLang.term -> StackLang.program =
    function
    | TreeLang.Num n -> [StackLang.Num n]
    | TreeLang.Add (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Add]
    | TreeLang.Sub (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Sub]

end
