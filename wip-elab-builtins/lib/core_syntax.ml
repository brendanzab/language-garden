type tm =
  | Item_var of string

  | Type

  | Bool_type
  | I32_type
  | I64_type
  | F64_type

  | Bool_lit of bool
  | I32_lit of int32
  | I64_lit of int64
  | F64_lit of float

type item =
  | Def of { label : string; ty : tm; tm : tm }


let pp_tm ppf =
  function
  | Item_var name -> Format.pp_print_string ppf name

  | Type -> Format.pp_print_string ppf "builtin.Type"

  | Bool_type -> Format.pp_print_string ppf "builtin.Bool"
  | I32_type -> Format.pp_print_string ppf "builtin.I32"
  | I64_type -> Format.pp_print_string ppf "builtin.I64"
  | F64_type -> Format.pp_print_string ppf "builtin.F64"

  | Bool_lit true -> Format.pp_print_string ppf "builtin.true"
  | Bool_lit false -> Format.pp_print_string ppf "builtin.false"
  | I32_lit x -> Format.pp_print_int ppf (Int32.to_int x)
  | I64_lit x -> Format.pp_print_int ppf (Int64.to_int x)
  | F64_lit x -> Format.pp_print_float ppf x
