(** Based on Figure 1.3 in “The Algorithmic Beauty of Plants” *)
module Algae : LSystem.S = struct

  module Symbol = struct

    (** Cytological state of a cell *)
    type t = [
      | `A  (** Large cell, ready to divide *)
      | `B  (** Small cell *)
    ]

    let to_string = function
      | `A -> "a"
      | `B -> "b"

  end

  let axiom = [`B]

  let rules =
    function
    | `A -> [`A; `B]  (* Divide *)
    | `B -> [`A]      (* Grow *)

end


(** Based on Figure 1.4 in “The Algorithmic Beauty of Plants” *)
module Filament : LSystem.S = struct

  (** A cell in a filament of Anabaena catenula *)
  module Symbol = struct

    (** Cytological state of a cell *)
    type size = [
      | `A  (** Long cell, ready to divide *)
      | `B  (** Short sell *)
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

  let rules =
    function
    | `A, `R -> [`A, `L; `B, `R]  (* Divide right *)
    | `A, `L -> [`B, `L; `A, `R]  (* Divide left *)
    | `B, `R -> [`A, `R]          (* Grow right *)
    | `B, `L -> [`A, `L]          (* Grow left *)

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
  let rules =
    function
    | `Line ->
        [
          `Line; `Right; `Line; `Left; `Line; `Left; `Line;
          `Line; `Right; `Line; `Right; `Line; `Left; `Line;
        ]
    | s -> [s]

end


(** From the {{:https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_tree}
    L-system page} on Wikipedia. *)
module BinaryTree : LSystem.S = struct

  module Symbol = struct

    type t = [
      | `Bud
      | `Branch
      | `Push
      | `Pop
    ]

    let to_string =
      function
      | `Bud -> "0"
      | `Branch -> "1"
      | `Push -> "["
      | `Pop -> "]"

  end

  let axiom = [`Bud]

  let rules =
    function
    | `Branch -> [`Branch; `Branch]              (* Grow the branch *)
    | `Bud -> [`Branch; `Push; `Bud; `Pop; `Bud] (* Split a bud into a branch and two buds *)
    | s -> [s]

end


(** From the {{:https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_tree}
    L-system page} on Wikipedia. *)
module CantorSet : LSystem.S = struct

  module Symbol = struct

    type t = [
      | `A  (** Draw forward *)
      | `B  (** Move forward *)
    ]

    let to_string =
      function
      | `A -> "A"
      | `B -> "B"

  end

  let axiom = [`A]

  let rules =
    function
    | `A -> [`A; `B; `A]
    | `B -> [`B; `B; `B]

end
