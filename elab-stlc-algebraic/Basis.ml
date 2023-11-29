module Void = struct

  type t = |

  let absurd : t -> 'a =
    function  _ -> .

  let equal : t -> t -> bool =
    function _ -> .

  let compare : t -> t -> int =
    function _ -> .

  let to_string : t -> string =
    function _ -> .

end
