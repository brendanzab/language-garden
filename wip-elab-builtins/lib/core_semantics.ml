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

and head =
  | ItemVar of string

and elim = |


let eval (items : string -> value) : Core_syntax.tm -> value =
  function
  | ItemVar name -> items name

  | Type -> Type

  | BoolType -> BoolType
  | I32Type -> I32Type
  | I64Type -> I64Type
  | F64Type -> F64Type

  | BoolLit x -> BoolLit x
  | I32Lit x -> I32Lit x
  | I64Lit x -> I64Lit x
  | F64Lit x -> F64Lit x


let rec quote : value -> Core_syntax.tm =
  function
  | Neutral (head, spine) -> quote_spine (quote_head head) spine

  | Type -> Type

  | BoolType -> BoolType
  | I32Type -> I32Type
  | I64Type -> I64Type
  | F64Type -> F64Type

  | BoolLit x -> BoolLit x
  | I32Lit x -> I32Lit x
  | I64Lit x -> I64Lit x
  | F64Lit x -> F64Lit x

and quote_head : head -> Core_syntax.tm =
  function
  | ItemVar name -> ItemVar name

and quote_spine head =
  function
  | [] -> head


let rec is_convertible (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | Neutral (h1, []), Neutral (h2, []) ->
      is_convertible_head h1 h2

  | Type, Type
  | BoolType, BoolType
  | I32Type, I32Type
  | I64Type, I64Type
  | F64Type, F64Type -> true

  | BoolLit x1, BoolLit x2 -> x1 = x2
  | I32Lit x1, I32Lit x2 -> x1 = x2
  | I64Lit x1, I64Lit x2 -> x1 = x2
  | F64Lit x1, F64Lit x2 -> x1 = x2

  | _, _ -> false

and is_convertible_head (h1 : head) (h2 : head) : bool =
  match h1, h2 with
  | ItemVar n1, ItemVar n2 -> n1 = n2
