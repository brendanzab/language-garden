(** Comparing isorecursive variant types with equirecursive variant types in OCaml.*)

[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-constructor"]

module type Isorecursive = sig

  type t
  type 'a layer

  val roll : t layer -> t
  val unroll : t -> t layer

end

module List1 (A : sig type t end) : Isorecursive = struct

  type 'a layer =
    | Cons of A.t * 'a
    | Nil of 'a

  (* Nominal variants are isorecursive, so the recursion must be
     guarded with a constructor *)

  type t = Fix of t layer

  let roll : t layer -> t = fun x -> Fix x
  let unroll : t -> t layer = fun (Fix x) -> x

end

module List2 (A : sig type t end) : Isorecursive = struct

  type 'a layer = [
    | `Cons of A.t * 'a
    | `Nil of 'a
  ]

  (* Polymorphic variants are equirecursive, so unguarded
     recursion in type aliases is allowed *)

  type t = t layer

  (* Equirecursion means that roll and unroll are the identity function *)

  let roll : t layer -> t = Fun.id
  let unroll : t -> t layer = Fun.id

end
