type ty =
  | Text_ty
  | Bool_ty
  | Int_ty

type value =
  | Text_lit of string
  | Bool_lit of bool
  | Int_lit of int

type t =
  | Text_eq
  | Text_concat
  | Bool_eq
  | Bool_not
  | Int_eq
  | Int_add
  | Int_sub
  | Int_mul
  | Int_neg

let name (prim : t) : string =
  match prim with
  | Text_eq -> "text-eq"
  | Text_concat -> "text-concat"
  | Bool_eq -> "bool-eq"
  | Bool_not -> "bool-not"
  | Int_eq -> "int-eq"
  | Int_add -> "int-add"
  | Int_sub -> "int-sub"
  | Int_mul -> "int-mul"
  | Int_neg -> "int-neg"

let ty (prim : t) : ty list * ty =
  match prim with
  | Text_eq -> [Text_ty; Text_ty], Bool_ty
  | Text_concat -> [Text_ty; Text_ty], Text_ty
  | Bool_eq -> [Bool_ty; Bool_ty], Bool_ty
  | Bool_not -> [Bool_ty], Bool_ty
  | Int_eq -> [Int_ty; Int_ty], Bool_ty
  | Int_add -> [Int_ty; Int_ty], Int_ty
  | Int_sub -> [Int_ty; Int_ty], Int_ty
  | Int_mul -> [Int_ty; Int_ty], Int_ty
  | Int_neg -> [Int_ty], Int_ty

let app (prim : t) : value list -> value =
  match prim with
  | Text_eq -> fun[@warning "-partial-match"] [Text_lit x; Text_lit y] -> Bool_lit (String.equal x y)
  | Text_concat -> fun[@warning "-partial-match"] [Text_lit x; Text_lit y] -> Text_lit (String.cat x y)
  | Bool_eq -> fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (Bool.equal t1 t2)
  | Bool_not -> fun[@warning "-partial-match"] [Bool_lit t1] -> Bool_lit (Bool.not t1)
  | Int_eq -> fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (Int.equal t1 t2)
  | Int_add -> fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.add t1 t2)
  | Int_sub -> fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.sub t1 t2)
  | Int_mul -> fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.mul t1 t2)
  | Int_neg -> fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (Int.neg t1)
