(** Demonstration local datatype definitions in OCaml *)

let test : int =
  let open struct

    type tree =
      | Leaf of int
      | Branch of tree * tree

    let rec size (t : tree) : int =
      match t with
      | Leaf _ -> 1
      | Branch (l, r) -> size l + size r

  end in

  size (Branch (Branch (Leaf 1, Leaf 3), Leaf 5))

let () =
  assert (test = 3)


(** This syntax might be allowed in the future:

    {[
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
    ]}

    See {{: https://github.com/ocaml/ocaml/pull/14040} Allow mostly arbitrary
    structure items in [let] expressions} for more information.
*)

(** Returning a locally defined type is an error:

    {[
      let test =
        let open struct
          type t = Foo of int
        end in
        Foo 3 (* Error: This constructor has type t but an expression was expected of type 'a
                        The type constructor t would escape its scope *)
    ]}
*)
