(** From the {{:https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_tree}
    L-system page} on Wikipedia. *)


type symbol =
  | Bud
  | Branch
  | Push
  | Pop

let axiom = [Bud]

let rules =
  function
  | Branch -> [Branch; Branch]            (* Grow the branch *)
  | Bud -> [Branch; Push; Bud; Pop; Bud]  (* Split a bud into a branch and two buds *)
  | s -> [s]


let string_of_symbol =
  function
  | Bud -> "0"
  | Branch -> "1"
  | Push -> "["
  | Pop -> "]"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""
