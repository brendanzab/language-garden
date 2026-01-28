let point x y :=
  { x := x; y := y };

let add p1 p2 := point (p1.x + p2.x) (p1.y + p2.y);
let sub p1 p2 := point (p1.x - p2.x) (p1.y - p2.y);

let _ :=
  add (point 1 2) (point 3 4);

let apply x :=
  match x with {
    | [incr := x] => x + 1
    | [decr := x] => x - 1
    | [square := x] => x * x
  };

apply [incr := 1]
