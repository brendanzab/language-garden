type term =
  | Num of int
  | Add of term * term
  | Sub of term * term

module Semantics = struct

  type value = int

  let rec eval : term -> value =
    function
    | Num n -> n
    | Add (n1, n2) -> eval n1 + eval n2
    | Sub (n1, n2) -> eval n1 - eval n2

end
