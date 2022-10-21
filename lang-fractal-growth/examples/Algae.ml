(** Based on Figure 1.3 in “The Algorithmic Beauty of Plants” *)


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
