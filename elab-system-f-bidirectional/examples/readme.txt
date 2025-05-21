let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

always [Int -> Int] (id [Int])
