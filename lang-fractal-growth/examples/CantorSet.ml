(** From the {{:https://en.wikipedia.org/wiki/L-system#Example_2:_Fractal_(binary)_command}
    L-system page} on Wikipedia. *)


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
