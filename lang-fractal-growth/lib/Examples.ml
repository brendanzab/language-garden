(** Based on Figure 1.3 in “The Algorithmic Beauty of Plants” *)
module Algae : LSystem.S = struct

  module Symbol = struct

    type t = [`A | `B]

    let to_string = function
      | `A -> "a"
      | `B -> "b"

  end

  let axiom = [`B]

  let rules = function
    | `A -> [`A; `B]
    | `B -> [`A]

end


(** Based on Figure 1.4 in “The Algorithmic Beauty of Plants” *)
module Filament : LSystem.S = struct

  (** A cell in a filament of Anabaena catenula *)
  module Symbol = struct

    (** Cytological state of a cell *)
    type size = [
      | `A (** Long, ready to divide *)
      | `B (** Short, not yet ready to divide *)
    ]

    (** Where new cells will be produced *)
    type polarity = [
      | `L  (** Divide to the left *)
      | `R  (** Divide to the right *)
    ]

    (** The state of a cell in a multicellular filament *)
    type t = size * polarity

    let to_string =
      function
      | `A, `R -> "(-->)"
      | `B, `R -> "(->)"
      | `A, `L -> "(<--)"
      | `B, `L -> "(<-)"

  end

  let axiom = [`A, `R]

  let rules = function
    | `A, `R -> [`A, `L; `B, `R]
    | `A, `L -> [`B, `L; `A, `R]
    | `B, `R -> [`A, `R]
    | `B, `L -> [`A, `L]

end


(** A turtle graphics command *)
module TurtleCommand = struct

  type t = [
    | `Left   (** Turn left by an angle {i δ} *)
    | `Right  (** Turn right by an angle {i δ} *)
    | `Line   (** Move forward a distance {i d}, drawing a line *)
    | `Space  (** Move forward a distance {i d}, leaving a space *)
  ]

  let to_string =
    function
    | `Left -> "+"
    | `Right -> "-"
    | `Line -> "F"
    | `Space -> "f"

  (* TODO: Graphical interpretation? *)

end


(** Based on Figure 1.6 in “The Algorithmic Beauty of Plants” *)
module KochIsland : LSystem.S = struct

  module Symbol = TurtleCommand

  (** Start with a square *)
  let axiom = [
    `Line; `Right;
    `Line; `Right;
    `Line; `Right;
    `Line;
  ]

  (** Grow a branch for each line on the predecessor *)
  let rules = function
    | `Line ->
        [
          `Line; `Right; `Line; `Left; `Line; `Left; `Line;
          `Line; `Right; `Line; `Right; `Line; `Left; `Line;
        ]
    | s -> [s]

end
