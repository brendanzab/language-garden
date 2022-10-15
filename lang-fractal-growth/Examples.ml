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
