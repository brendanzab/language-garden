let apply x :=
  match x with
  | [incr := x] => x + 1
  | [decr := x] => x - 1
  | [square := x] => x * x
  end;

apply [incr := 1]
