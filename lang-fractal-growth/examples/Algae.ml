(** Based on Figure 1.3 in “The Algorithmic Beauty of Plants” *)


(* $MDX part-begin=grammar *)
(** Cytological state of a cell *)
type symbol =
  | A             (** Large cell, ready to divide *)
  | B             (** Small cell *)

let axiom =
  [B]             (* Seed with a small cell *)

let rules =
  function
  | A -> [A; B]   (* Divide *)
  | B -> [A]      (* Grow *)
(* $MDX part-end *)


(** {1 String interpretation} *)

let string_of_symbol =
  function
  | A -> "a"
  | B -> "b"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""
