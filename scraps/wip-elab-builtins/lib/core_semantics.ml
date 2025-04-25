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

and head =
  | Item_var of string

and elim = |


let eval (items : string -> value) : Core_syntax.tm -> value =
  function
  | Item_var name -> items name

  | Type -> Type

  | Bool_type -> Bool_type
  | I32_type -> I32_type
  | I64_type -> I64_type
  | F64_type -> F64_type

  | Bool_lit x -> Bool_lit x
  | I32_lit x -> I32_lit x
  | I64_lit x -> I64_lit x
  | F64_lit x -> F64_lit x


let rec quote : value -> Core_syntax.tm =
  function
  | Neutral (head, spine) -> quote_spine (quote_head head) spine

  | Type -> Type

  | Bool_type -> Bool_type
  | I32_type -> I32_type
  | I64_type -> I64_type
  | F64_type -> F64_type

  | Bool_lit x -> Bool_lit x
  | I32_lit x -> I32_lit x
  | I64_lit x -> I64_lit x
  | F64_lit x -> F64_lit x

and quote_head : head -> Core_syntax.tm =
  function
  | Item_var name -> Item_var name

and quote_spine head =
  function
  | [] -> head


let rec is_convertible (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | Neutral (h1, []), Neutral (h2, []) ->
      is_convertible_head h1 h2

  | Type, Type
  | Bool_type, Bool_type
  | I32_type, I32_type
  | I64_type, I64_type
  | F64_type, F64_type -> true

  | Bool_lit x1, Bool_lit x2 -> x1 = x2
  | I32_lit x1, I32_lit x2 -> x1 = x2
  | I64_lit x1, I64_lit x2 -> x1 = x2
  | F64_lit x1, F64_lit x2 -> x1 = x2

  | _, _ -> false

and is_convertible_head (h1 : head) (h2 : head) : bool =
  match h1, h2 with
  | Item_var n1, Item_var n2 -> n1 = n2
