(** Some examples of abstract data types with their associated properties *)

[@@@warning "-unused-value-declaration"]

module type Stack = sig

  type t
  type elem

  val empty : t
  val push : elem -> t -> t
  val pop : elem -> t -> (elem * t) option
  val is_empty : elem -> t -> bool

  (** Equations:

      - [is_empty empty]        = [true]
      - [is_empty (push x s)]   = [false]
      - [pop empty]             = [None]
      - [pop (push x s)]        = [Some (x, s)]
  *)

end

module type Set = sig

  type t
  type elem

  val empty : t
  val add : elem -> t -> t
  val remove : elem -> t -> t
  val mem : elem -> t -> bool

  (** Equations:

      - [mem x empty]           = [false]
      - [mem x (add x b)]       = [true]
      - [mem x (add x' b)]      = [mem x b]             where [x <> x']
      - [remove x empty]        = [empty]
      - [remove x (add x b)]    = [remove x b]
      - [remove x (add x' b)]   = [add x' (remove x b)] where [x <> x']

    From “The algebraic specification of abstract data types” by Guttag and Horning
  *)

end

(* TODO: Test properties with QuickCheck *)

(*
module Set_properties (S : Set with type elem = int) : sig

  val tests : QCheck2.Test.t list

end = struct

  module Gen = QCheck2.Gen
  module Test = QCheck2.Test

  let set_gen : S.t Gen.t =
    let open Gen in
    let+ xs = list nat (* TODO: generate list of unique elements *) in
    List.fold_right S.add xs S.empty

  let mem_empty x                   = S.mem x S.empty = false
  let mem_add_same (x, s)           = S.mem x (S.add x s) = true
  let mem_add_different (x, x', s)  = QCheck2.assume (x <> x'); S.mem x (S.add x' s) = S.mem x s

  let tests = [
    Test.make Gen.int mem_empty;
    Test.make Gen.(tup2 int set_gen) mem_add_same;
    Test.make Gen.(tup3 int int set_gen) mem_add_different;
  ]

end
*)
