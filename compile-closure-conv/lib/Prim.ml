(** {0 Primitive operations}

    These operations are shared between the different intermediate languages.
*)

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


(* TODO: define evaluation of primitives once in this module *)
