(** {0 Fresh variable generation} *)

(** A fresh variable generator *)
module type S = sig

  (** Globally unique id *)
  type t = private int

  (** Returns a fresh id *)
  val fresh : unit -> t

  (** Total ordering of ids *)
  val compare : t -> t -> int

  (** Convert an id to an integer *)
  val to_int : t -> int

end

(** Build a new namespace of globally unique ids *)
module Make () : S = struct

  type t = int

  let next_id = ref 0

  let fresh () =
    let id = !next_id in
    incr next_id;
    id

  let compare = Int.compare

  let to_int id = id

end
