module type S = sig

  type t = private int

  val fresh : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_int : t -> int

end

module Make () : S = struct

  type t = int

  let next_id = ref 0

  let fresh () =
    let id = !next_id in
    incr next_id;
    id

  let equal = Int.equal
  let compare = Int.compare
  let to_int id = id

end
