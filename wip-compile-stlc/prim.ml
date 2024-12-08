type ty =
  | Bool_ty
  | Int_ty

type value =
  | Bool_lit of bool
  | Int_lit of int

type t =
  | Bool_eq
  | Bool_not
  | Int_eq
  | Int_add
  | Int_sub
  | Int_mul
  | Int_neg

let name (prim : t) : string =
  match prim with
  | Bool_eq -> "bool-eq"
  | Bool_not -> "bool-not"
  | Int_eq -> "int-eq"
  | Int_add -> "int-add"
  | Int_sub -> "int-sub"
  | Int_mul -> "int-mul"
  | Int_neg -> "int-neg"

let ty (prim : t) : ty list * ty =
  match prim with
  | Bool_eq -> [Bool_ty; Bool_ty], Bool_ty
  | Bool_not -> [Bool_ty], Bool_ty
  | Int_eq -> [Int_ty; Int_ty], Bool_ty
  | Int_add -> [Int_ty; Int_ty], Int_ty
  | Int_sub -> [Int_ty; Int_ty], Int_ty
  | Int_mul -> [Int_ty; Int_ty], Int_ty
  | Int_neg -> [Int_ty], Int_ty

let app (prim : t) : value list -> value =
  match prim with
  | Bool_eq -> fun[@warning "-partial-match"] [Bool_lit x; Bool_lit y] -> Bool_lit (x = y)
  | Bool_not -> fun[@warning "-partial-match"] [Bool_lit x] -> Bool_lit (not x)
  | Int_eq -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (x = y)
  | Int_add -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (x + y)
  | Int_sub -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (x - y)
  | Int_mul -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (x * y)
  | Int_neg -> fun[@warning "-partial-match"] [Int_lit x] -> Int_lit (-x)
