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

let of_name (name : string) : t option =
  match name with
  | "bool-eq" -> Some Bool_eq
  | "int-eq" -> Some Int_eq
  | "int-add" -> Some Int_add
  | "int-sub" -> Some Int_sub
  | "int-mul" -> Some Int_mul
  | "int-neg" -> Some Int_neg
  | _ -> None
