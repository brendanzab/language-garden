type _ index = int
type _ level = int
type _ size = int

type (_, 'a) t = 'a list

let empty = []

let get_index x env =
  List.nth env x

let size =
  List.length

let level_to_index size level =
  size - level - 1

let empty_size = 0
let bind_entry x env = x :: env
let bind_level size = size + 1
let next_level size = size
