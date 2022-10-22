(** Based on Figure 1.6 in “The Algorithmic Beauty of Plants” *)


(** Turtle command *)
type symbol =
  | Left    (** Turn left by an angle {i δ} *)
  | Right   (** Turn right by an angle {i δ} *)
  | Line    (** Move forward a distance {i d}, drawing a line *)

(** Start with a square *)
let axiom = [
  Line; Right;
  Line; Right;
  Line; Right;
  Line;
]

let rules =
  function
  (* Grow a branch for each line on the predecessor *)
  | Line ->
      [
        Line; Right; Line; Left; Line; Left; Line;
        Line; Right; Line; Right; Line; Left; Line;
      ]
  | s -> [s]


let string_of_symbol =
  function
  | Left -> "+"
  | Right -> "-"
  | Line -> "F"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""

(* TODO: Graphical interpretation? *)
