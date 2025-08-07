let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

let _ := id 3;
let _ := id id : [a] -> a -> a;
let _ := always [[a] -> a -> a] id 3;

-- Call a polymorphic argument with different types
let test (f : [a] -> a -> a) : [a] -> a -> a :=
  let _ := f 3;     -- integers
  let _ := f true;  -- boolean
  f f;              -- itself

test (fun x => x)
