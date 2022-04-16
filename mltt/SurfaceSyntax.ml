type term =
  | Name of string
  (* TODO: | Placeholder *)
  (* TODO: | Hole of string *)
  | Ann of term * term
  | Let of string option * term * term * term
  | Type
  | Arrow of term * term
  | FunctionType of string option * term * term
  | FunctionLit of string option * term option * term
  | RecordType of (string * term) list
  | RecordLit of (string * term) list
  | Unit
  | App of term * term list
  | Proj of term * string
