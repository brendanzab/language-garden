type binop =
  | Add
  | Sub

type tm =
  | Path of string list
  | Num_lit of string
  | Binop of tm * binop * tm

type item =
  | Use of { path : string list }
  | Def of { label : string; ty : tm option; tm : tm }
