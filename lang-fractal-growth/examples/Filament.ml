(** Based on Equation 1.1 in “The Algorithmic Beauty of Plants” *)


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
