type t =
  | BoolEq
  | IntEq
  | IntAdd
  | IntSub
  | IntMul
  | IntNeg

let name (prim : t) : string =
  match prim with
  | BoolEq -> "bool-eq"
  | IntEq -> "int-eq"
  | IntAdd -> "int-add"
  | IntSub -> "int-sub"
  | IntMul -> "int-mul"
  | IntNeg -> "int-neg"
