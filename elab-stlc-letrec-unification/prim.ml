type t =
  | Bool_eq
  | Int_eq
  | Int_add
  | Int_sub
  | Int_mul
  | Int_neg

let name (prim : t) : string =
  match prim with
  | Bool_eq -> "bool-eq"
  | Int_eq -> "int-eq"
  | Int_add -> "int-add"
  | Int_sub -> "int-sub"
  | Int_mul -> "int-mul"
  | Int_neg -> "int-neg"
