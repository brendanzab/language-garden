(** Based on Equation 1.1 in “The Algorithmic Beauty of Plants” *)


(* $MDX part-begin=grammar *)
(** Cytological state of a cell *)
type size =
  | A   (** Long cell, ready to divide *)
  | B   (** Short cell *)

(** Where new cells will be produced *)
type polarity =
  | L   (** Divide to the left *)
  | R   (** Divide to the right *)

(** The state of a cell in a filament of Anabaena catenula *)
type symbol = size * polarity

let axiom = [A, R]

let rules =
  function
  | A, R -> [A, L; B, R]    (* Divide right *)
  | A, L -> [B, L; A, R]    (* Divide left *)
  | B, R -> [A, R]          (* Grow right *)
  | B, L -> [A, L]          (* Grow left *)
(* $MDX part-end *)


let string_of_symbol =
  function
  | A, R -> "(-->)"
  | B, R -> "(->)"
  | A, L -> "(<--)"
  | B, L -> "(<-)"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""
