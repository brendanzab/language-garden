(** Natural numbers, represented internally as integers for improved performance *)

[@@@warning "-unused-value-declaration"]

module Nat : sig

  type t [@@immediate]

  val zero : t
  val succ : t -> t

  val recur : t -> zero:(unit -> 'a) -> succ:(t -> 'a) -> 'a

  val intro : [ `Zero | `Succ of t ] -> t
  val elim : t -> [ `Zero | `Succ of t ]

  val add : t -> t -> t
  val mul : t -> t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val of_int : int -> t
  val to_int : t -> int

end = struct

  type t = int

  let zero = 0

  let succ n =
    if n <> Int.max_int then n + 1 else
      invalid_arg "Nat.succ"

  let recur n ~zero ~succ =
    match n with
    | 0 -> zero ()
    | n -> succ (n - 1)

  let intro n =
    match n with
    | `Zero -> zero
    | `Succ n -> succ n

  let elim n =
    match n with
    | 0 -> `Zero
    | n -> `Succ (n - 1)

  let add n1 n2 =
    n1 + n2 (* FIXME: overflow *)

  let mul n1 n2 =
    n1 * n2 (* FIXME: overflow *)

  let compare n1 n2 = Int.compare n1 n2
  let equal n1 n2 = Int.equal n1 n2

  let of_int i =
    if i >= 0 then i else
      invalid_arg "Nat.of_int"

  let to_int n = n

end

let rec fact (x : Nat.t) : Nat.t =
  Nat.recur x
    ~zero:(fun () -> Nat.succ Nat.zero)
    ~succ:(fun x' -> Nat.mul x (fact x'))

let rec fact' (x : Nat.t) : Nat.t =
  match Nat.elim x with
  | `Zero -> Nat.succ Nat.zero
  | `Succ x' -> Nat.mul x (fact' x')
