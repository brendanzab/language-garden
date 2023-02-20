(** {0 Primitive operations} *)

type t = [
  | `Neg
  | `Add
  | `Sub
  | `Mul
]

let to_string : t -> string =
  function
  | `Neg -> "neg"
  | `Add -> "add"
  | `Sub -> "sub"
  | `Mul -> "mul"
