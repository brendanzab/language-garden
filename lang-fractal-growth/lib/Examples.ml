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


(** Based on Equation 1.1 in “The Algorithmic Beauty of Plants” *)
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

(** Based on Equation 1.7 in “The Algorithmic Beauty of Plants” *)
module Parametric : LSystem.S = struct

  (* Note that parametric L-Systems fall out of OCaml’s algebraic datatypes and
     pattern guards. No additional features are required! *)

  module Symbol = struct

    type t =
      | A of int * int
      | B of int
      | C

    let to_string =
      (* FIXME: could do with some spaces and/or punctuation between symbols *)
      function
      | A (x, y) -> Format.sprintf "A(%i, %i)" x y
      | B x -> Format.sprintf "B(%i)" x
      | C -> "C"

  end

  open Symbol

  let axiom = [B 2; A (4, 4)]

  let rules =
    function
    | A (x, y)  when y <= 3   -> [A (x * 2, x + y)]
    | A (x, y)  (* y > 3 *)   -> [B x; A (x / y, 0)]
    | B x       when x < 1    -> [C]
    | B x       (* x >= 1 *)  -> [B (x - 1)]
    | C                       -> [C]

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
