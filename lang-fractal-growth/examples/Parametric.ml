(** Based on Equation 1.7 in “The Algorithmic Beauty of Plants” *)

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


let string_of_symbol =
  (* FIXME: could do with some spaces and/or punctuation between symbols *)
  function
  | A (x, y) -> Format.sprintf "A(%i, %i)" x y
  | B x -> Format.sprintf "B(%i)" x
  | C -> "C"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat " "
