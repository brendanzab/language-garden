let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

let _ := id 3;
let _ := id id;
let _ := always id 3;

-- Call a polymorphic function with two different types
let test (f : [a] -> a -> a) : Bool :=
  let _ := f 3;
  f true;

test (fun x => x)
