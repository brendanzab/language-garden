let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

let _ := id [Int] 3;
let _ := id [[a] -> a -> a] id;
let _ := always [[a] -> a -> a] id [Int] 3;

-- Call a polymorphic argument with different types
let test (f : [a] -> a -> a) : [a] -> a -> a :=
  let _ := f [Int] 3;       -- integers
  let _ := f [Bool] true;   -- boolean
  f [[a] -> a -> a] f;      -- itself

test (fun x => x)
