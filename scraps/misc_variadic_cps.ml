(** Variadic functions using CPS.

    https://okmij.org/ftp/Computation/extra-polymorphism.html
*)

let zero : type a b. (a list -> b) -> b =
  fun k -> k []

let succ : type a b c. ((a list -> b) -> c) -> (a list -> b) -> a -> c =
  fun n k x ->
    n (fun v -> k (x :: v))

let poly : type a b. ((a list -> a list) -> b) -> b =
  fun sel ->
    sel (fun x -> x)

let () = begin

    assert (poly zero = []);
    assert (poly (succ zero) 1 = [1]);
    assert (poly (succ (succ zero)) 1 2 = [1; 2]);
    assert (poly (succ (succ (succ zero))) 'a' 'b' 'c' = ['a'; 'b'; 'c']);

end
