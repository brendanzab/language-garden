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


module Symbol = struct

  type t = symbol

  let to_string =
    function
    | Bud -> "0"
    | Branch -> "1"
    | Push -> "["
    | Pop -> "]"

end
