(** This file demonstrates how to use laziness to patch up a data structure
    after-the-fact in a single traversal, without using mutable references.

    This technique was described by R.S. Bird’ in {{: https://gwern.net/doc/cs/haskell/1984-bird.pdf}
    “Using Circular Programs to Eliminate Multiple Traversals of Data”} and
    could be used as a stepping stone towards implementing {{: https://emilaxelsson.github.io/documents/axelsson2013using.pdf}
    “Using circular programs for higher-order syntax: functional pearl”} in OCaml.
*)

module Tree = struct

  type 'a t =
    | Tip of 'a
    | Fork of 'a t * 'a t

  let rec map (type a b) (f : 'a -> 'b) (t : 'a t) : 'b t =
    match t with
    | Tip n -> Tip (f n)
    | Fork (l, r) -> Fork (map f l, map f r)

  let rec equal (type a b) (tip : 'a -> 'b -> bool) (t1 : 'a t) (t2 : 'b t) : bool =
    match t1, t2 with
    | Tip n1, Tip n2 -> tip n1 n2
    | Fork (l1, r1), Fork (l2, r2) -> equal tip l1 l2 && equal tip r1 r2
    | _, _ -> false

end


module Single_traversal_procedural = struct

  (** Replace each node with the minimum value *)
  let transform (t : int ref Tree.t) : unit =
    let tips = Dynarray.create () in

    let rec tmin (t : int ref Tree.t) =
      match t with
      | Tip n -> Dynarray.add_last tips n; !n
      | Fork (l, r) -> Int.min (tmin l) (tmin r)
    in

    let m = tmin t in
    tips |> Dynarray.iter (fun n -> n := m)

end


module Multiple_traversals_functional = struct

  let rec tmin (t : int Tree.t) : int =
    match t with
    | Tip n -> n
    | Fork (l, r) -> Int.min (tmin l) (tmin r)

  let rec replace (t : int Tree.t) (n : int) : int Tree.t =
    match t with
    | Tip _ -> Tip n
    | Fork (l, r) -> Fork (replace l n, replace r n)

  (** Replace each node with the minimum value in two traversals *)
  let transform (t : int Tree.t) : int Tree.t =
    replace t (tmin t)

end


module Single_traversal_functional = struct

  let rec repmin (t : int Tree.t) (m : int Lazy.t) : int Lazy.t Tree.t * int =
    match t with
    | Tip n -> (Tip m, n)
    | Fork (l, r) ->
        let (t1, m1) = repmin l m in
        let (t2, m2) = repmin r m in
        (Fork (t1, t2), Int.min m1 m2)

  let transform (t : int Tree.t) : int Lazy.t Tree.t =
    let rec p = lazy (repmin t (Lazy.map snd p)) in
    fst (Lazy.force p)

end


module Tests = struct

  Printexc.record_backtrace true

  let tree = Tree.Fork (Tip 3, Fork (Tip 2, Tip 4))
  let expected = Tree.Fork (Tip 2, Fork (Tip 2, Tip 2))

  let () =
    let tree = Tree.map ref tree in
    Single_traversal_procedural.transform tree;
    assert (Tree.equal (fun n1 n2 -> !n1 = n2) tree expected)

  let () =
    let result = Multiple_traversals_functional.transform tree in
    assert (Tree.equal ( = ) result expected)

  let () =
    let result = Single_traversal_functional.transform tree in
    assert (Tree.equal (fun n1 n2 -> Lazy.force n1 = n2) result expected)

end
