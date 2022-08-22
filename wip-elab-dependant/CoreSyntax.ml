type index = int

type term =
  | Var of index
  | Let of term * term * term
  | UnivType
  | TypeFunction of term * term
  | TypeRecord of (string * term) list
  | FunctionLit of term * term
  | FunctionApp of term * term
  | RecordLit of (string * term) list
  | RecordProj of term * string
