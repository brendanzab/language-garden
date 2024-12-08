type ty =
  | BoolTy
  | IntTy

type value =
  | BoolLit of bool
  | IntLit of int

type t =
  | BoolEq
  | BoolNot
  | IntEq
  | IntAdd
  | IntSub
  | IntMul
  | IntNeg

let name (prim : t) : string =
  match prim with
  | BoolEq -> "bool-eq"
  | BoolNot -> "bool-not"
  | IntEq -> "int-eq"
  | IntAdd -> "int-add"
  | IntSub -> "int-sub"
  | IntMul -> "int-mul"
  | IntNeg -> "int-neg"

let ty (prim : t) : ty list * ty =
  match prim with
  | BoolEq -> [BoolTy; BoolTy], BoolTy
  | BoolNot -> [BoolTy], BoolTy
  | IntEq -> [IntTy; IntTy], BoolTy
  | IntAdd -> [IntTy; IntTy], IntTy
  | IntSub -> [IntTy; IntTy], IntTy
  | IntMul -> [IntTy; IntTy], IntTy
  | IntNeg -> [IntTy], IntTy

let app (prim : t) : value list -> value =
  match prim with
  | BoolEq -> fun[@warning "-partial-match"] [BoolLit x; BoolLit y] -> BoolLit (x = y)
  | BoolNot -> fun[@warning "-partial-match"] [BoolLit x] -> BoolLit (not x)
  | IntEq -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (x = y)
  | IntAdd -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (x + y)
  | IntSub -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (x - y)
  | IntMul -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (x * y)
  | IntNeg -> fun[@warning "-partial-match"] [IntLit x] -> IntLit (-x)
