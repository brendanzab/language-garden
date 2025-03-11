(** From the {{: https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_command}
    L-system page} on Wikipedia. *)


(** Drawing command *)
type symbol =
  | Draw   (** Draw forward *)
  | Move   (** Move forward *)

let axiom = [Draw]

let rules =
  function
  | Draw -> [Draw; Move; Draw]
  | Move -> [Move; Move; Move]


let string_of_symbol =
  function
  | Draw -> "A"
  | Move -> "B"

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""
