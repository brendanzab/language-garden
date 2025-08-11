(** Demonstration of Landin’s Knot, an approach to encoding general recursion
    using higher-order references and backpatching.

    {{: https://doi.org/10.1093/comjnl/6.4.308} The Mechanical Evaluation of Expressions}
*)

let fix (type a b) (f : (a -> b) -> a -> b) : a -> b =
  let recur = ref (fun _ -> assert false) in  (* create a reference to be used inside the recursive function *)
  let knot = f (fun x -> !recur x) in         (* create the recursive function *)
  recur := knot;                              (* backpatch the recursive function to call itself *)
  knot                                        (* return the recursive function *)

let fact : int -> int =
  fix @@ fun fact x ->
    match x with
    | 0 -> 1
    | x -> x * fact (x - 1)

let length (type a) : a list -> int =
  fix @@ fun length xs ->
    match xs with
    | [] -> 0
    | _ :: xs -> 1 + length xs


let () = begin

  assert (fact 0 = 1);
  assert (fact 1 = 1);
  assert (fact 2 = 2);
  assert (fact 3 = 6);
  assert (fact 4 = 24);
  assert (fact 5 = 120);
  assert (fact 6 = 720);
  assert (fact 7 = 5040);
  assert (fact 8 = 40320);
  assert (fact 9 = 362880);

  assert (length [] = 0);
  assert (length [1] = 1);
  assert (length [1; 2; 3; 4; 5] = 5);

end
