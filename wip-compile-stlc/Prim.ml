(** Primitive operations *)
type t =
  | BoolEq
  | BoolNot
  | IntEq
  | IntAdd
  | IntSub
  | IntMul
  | IntNeg

let to_string (prim : t) : string =
  match prim with
  | BoolEq -> "bool-eq"
  | BoolNot -> "bool-not"
  | IntEq -> "int-eq"
  | IntAdd -> "int-add"
  | IntSub -> "int-sub"
  | IntMul -> "int-mul"
  | IntNeg -> "int-neg"
