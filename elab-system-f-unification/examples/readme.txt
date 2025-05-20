let id [a] (x : a) := x;
let always [a] (x : a) [b] (y : b) := x;

let _ := id 3;
let _ := id id;
let _ := always id;
let _ := always (id [Int]) 3;

true
