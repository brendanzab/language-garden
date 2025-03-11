(** Demonstration of Landin’s Knot, an approach to encoding general recursion
    using higher-order references and backpatching.

    {{: https://doi.org/10.1093/comjnl/6.4.308} The Mechanical Evaluation of Expressions}
*)

[@@@warning "-unused-value-declaration"]

let knot (type a b) (f : (a -> b) -> a -> b) : a -> b =
  let recur = ref (fun _ -> assert false) in  (* create a reference to be used inside the recursive function *)
  let fix = f (fun x -> !recur x) in          (* create the recursive function *)
  recur := fix;                               (* backpatch the recursive function to call itself *)
  fix                                         (* return the recursive function *)

let fact : int -> int =
  knot @@ fun fact x ->
    match x with
    | 0 -> 1
    | x -> x * fact (x - 1)

let length (type a) : a list -> int =
  knot @@ fun size xs ->
    match xs with
    | [] -> 0
    | _ :: xs -> 1 + size xs

let () =
  Seq.ints 0 |> Seq.take 10 |> Seq.iter @@ fun x ->
    Printf.printf "fact %i = %i\n" x (fact x)
