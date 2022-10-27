type value =
  | Neutral of head * elim list

  | Type

  | BoolType
  | I32Type
  | I64Type
  | F64Type

  | BoolLit of bool
  | I32Lit of int32
  | I64Lit of int64
  | F64Lit of float

and head = ItemVar of string
and elim = |

val eval : (string -> value) -> CoreSyntax.tm -> value

val quote : value -> CoreSyntax.tm

val is_convertible : value -> value -> bool
