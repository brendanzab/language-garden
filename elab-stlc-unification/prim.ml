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


module Test = struct

  type _ prim_fun =
    | Bool_eq : (bool -> bool -> bool) prim_fun
    | Int_eq : (int -> int -> bool) prim_fun
    | Int_add : (int -> int -> int) prim_fun
    | Int_sub : (int -> int -> int) prim_fun
    | Int_mul : (int -> int -> int) prim_fun
    | Int_neg : (int -> int) prim_fun

  let sem : type f. f prim_fun -> f = function
    | Bool_eq -> Bool.equal
    | Int_eq -> Int.equal
    | Int_add -> Int.add
    | Int_sub -> Int.sub
    | Int_mul -> Int.mul
    | Int_neg -> Int.neg

  let _ : bool = sem Int_eq 2 3

end
