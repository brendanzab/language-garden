type level = int

type closure
type telescope

type value =
  | Neutral of neu
  | UnivType
  | TypeFunction of value * closure
  | TypeRecord of string list * telescope
  | FunctionLit of value * closure
  | RecordLit of (string * value) list
and neu =
  | Var of level
  | FunctionApp of neu * value
  | RecordProj of neu * string

type env =
  value list

exception Error of string

val eval : env -> CoreSyntax.term -> value
val quote : level -> value -> CoreSyntax.term
val norm : env -> CoreSyntax.term -> CoreSyntax.term

val closure_app : closure -> value -> value
val telescope_uncons : telescope -> (value * (value -> telescope)) option

val function_app : value -> value -> value
val record_proj : value -> string -> value

val is_convertible : level -> value -> value -> bool
