type _ index = int
type _ level = int
type _ size = int

type (_, 'a) t = 'a list

let empty = []

let bind_entry x env =
  x :: env

let lookup x env =
  List.nth env x

let size env =
  List.length env

let entry_index a env =
  let rec go i = function
    | [] -> None
    | x :: env -> if x = a then Some i else go (i + 1) env in
  go 0 env

let empty_size = 0

let bind_level size =
  size + 1

let next_level size =
  size

let level_to_index size level =
  size - level - 1
