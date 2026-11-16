(** Demonstration local datatype definitions in OCaml *)

let test : int =
  let type tree =
      | Leaf of int
      | Branch of tree * tree
  in
  let rec size (t : tree) : int =
    match t with
    | Leaf _ -> 1
    | Branch (l, r) -> size l + size r
  in

  size (Branch (Branch (Leaf 1, Leaf 3), Leaf 5))

let () =
  assert (test = 3)

(** Returning a locally defined type is an error:

    {[
      let test =
        let type t = Foo of int in
        Foo 3 (* Error: This constructor has type t but an expression was expected of type 'a
                        The type constructor t would escape its scope *)
    ]}
*)
