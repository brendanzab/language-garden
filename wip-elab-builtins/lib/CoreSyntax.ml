type tm =
  | ItemVar of string

  | Type

  | BoolType
  | I32Type
  | I64Type
  | F64Type

  | BoolLit of bool
  | I32Lit of int32
  | I64Lit of int64
  | F64Lit of float

type item =
  | Def of { label : string; ty : tm; tm : tm }


let pp_tm ppf =
  function
  | ItemVar name -> Format.pp_print_string ppf name

  | Type -> Format.pp_print_string ppf "builtin.Type"

  | BoolType -> Format.pp_print_string ppf "builtin.Bool"
  | I32Type -> Format.pp_print_string ppf "builtin.I32"
  | I64Type -> Format.pp_print_string ppf "builtin.I64"
  | F64Type -> Format.pp_print_string ppf "builtin.F64"

  | BoolLit true -> Format.pp_print_string ppf "builtin.true"
  | BoolLit false -> Format.pp_print_string ppf "builtin.false"
  | I32Lit x -> Format.pp_print_int ppf (Int32.to_int x)
  | I64Lit x -> Format.pp_print_int ppf (Int64.to_int x)
  | F64Lit x -> Format.pp_print_float ppf x
