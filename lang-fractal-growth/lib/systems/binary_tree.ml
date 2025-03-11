(** From the {{: https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_tree}
    L-system page} on Wikipedia. *)


type symbol =
  | Apex
  | Branch
  | Push
  | Pop

let axiom = [Apex]

let rules =
  function
  | Branch -> [Branch; Branch]                 (* Grow the branch *)
  | Apex   -> [Branch; Push; Apex; Pop; Apex]  (* Split a bud into a branch and two buds *)
  (* TODO: Terminal symbols *)
  | s -> [s]


let string_of_symbol =
  function
  | Apex -> "0"
  | Branch -> "1"
  | Push -> "["
  | Pop -> "]"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""
