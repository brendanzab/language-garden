type value =
  | Neutral of head * elim list

  | Type

  | Bool_type
  | I32_type
  | I64_type
  | F64_type

  | Bool_lit of bool
  | I32_lit of int32
  | I64_lit of int64
  | F64_lit of float

and head = Item_var of string
and elim = |

val eval : (string -> value) -> Core_syntax.tm -> value

val quote : value -> Core_syntax.tm

val is_convertible : value -> value -> bool
