(** Based on Figure 1.3 in “The Algorithmic Beauty of Plants” *)
module Algae : LSystem.Grammar = struct

  (** Cytological state of a cell *)
  type cell =
    | A             (** Large cell, ready to divide *)
    | B             (** Small cell *)

  let axiom =
    [B]             (* Seed with a small cell *)

  let rules =
    function
    | A -> [A; B]   (* Divide *)
    | B -> [A]      (* Grow *)


  module Symbol = struct

    type t = cell

    let to_string = function
      | A -> "a"
      | B -> "b"

  end

end


(** Based on Equation 1.1 in “The Algorithmic Beauty of Plants” *)
module Filament : LSystem.Grammar = struct

  (** Cytological state of a cell *)
  type size =
    | A   (** Long cell, ready to divide *)
    | B   (** Short sell *)

  (** Where new cells will be produced *)
  type polarity =
    | L   (** Divide to the left *)
    | R   (** Divide to the right *)

  (** The state of a cell in a multicellular filament *)
  type cell = size * polarity

  let axiom = [A, R]

  let rules =
    function
    | A, R -> [A, L; B, R]    (* Divide right *)
    | A, L -> [B, L; A, R]    (* Divide left *)
    | B, R -> [A, R]          (* Grow right *)
    | B, L -> [A, L]          (* Grow left *)


  (** A cell in a filament of Anabaena catenula *)
  module Symbol = struct

    (** The state of a cell in a multicellular filament *)
    type t = cell

    let to_string =
      function
      | A, R -> "(-->)"
      | B, R -> "(->)"
      | A, L -> "(<--)"
      | B, L -> "(<-)"

  end

end


(** Based on Figure 1.6 in “The Algorithmic Beauty of Plants” *)
module KochIsland : LSystem.Grammar = struct

  (** Turtle command *)
  type command =
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


  module Symbol = struct

    type t = command

    let to_string =
      function
      | Left -> "+"
      | Right -> "-"
      | Line -> "F"

    (* TODO: Graphical interpretation? *)

  end

end

(** Based on Equation 1.7 in “The Algorithmic Beauty of Plants” *)
module Parametric : LSystem.Grammar = struct

  (* Note that parametric L-Systems fall out of OCaml’s algebraic datatypes and
     pattern guards. No additional features are required! *)

  type symbol =
    | A of int * int
    | B of int
    | C

  let axiom = [B 2; A (4, 4)]

  let rules =
    function
    | A (x, y)  when y <= 3   -> [A (x * 2, x + y)]
    | A (x, y)  (* y > 3 *)   -> [B x; A (x / y, 0)]
    | B x       when x < 1    -> [C]
    | B x       (* x >= 1 *)  -> [B (x - 1)]
    | C                       -> [C]


  module Symbol = struct

    type t = symbol

    let to_string =
      (* FIXME: could do with some spaces and/or punctuation between symbols *)
      function
      | A (x, y) -> Format.sprintf "A(%i, %i)" x y
      | B x -> Format.sprintf "B(%i)" x
      | C -> "C"

  end

end


(** From the {{:https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_tree}
    L-system page} on Wikipedia. *)
module BinaryTree : LSystem.Grammar = struct

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

end


(** From the {{:https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_command}
    L-system page} on Wikipedia. *)
module CantorSet : LSystem.Grammar = struct

  type command =
    | Draw   (** Draw forward *)
    | Move   (** Move forward *)


  let axiom = [Draw]

  let rules =
    function
    | Draw -> [Draw; Move; Draw]
    | Move -> [Move; Move; Move]


  module Symbol = struct

    type t = command

    let to_string =
      function
      | Draw -> "A"
      | Move -> "B"

  end

end
