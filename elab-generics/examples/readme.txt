let id [A] (x : A) : A := x;
let const [A, B] (x : A) (y : B) : A := x;

let flip [A, B, C] (f : A -> B -> C) : B -> A -> C :=
  fun x y => f y x;

flip const true id 45
