(** {0 Fresh variable generation} *)

(** A fresh variable generator *)
module type S = sig

  (** Globally unique id *)
  type t = private int

  (** Returns a fresh id *)
  val fresh : string -> t

  (** Total ordering of ids *)
  val compare : t -> t -> int

  (** Convert an id to an integer *)
  val to_int : t -> int

  (** Recover the name that an id was generated with (not guaranteed to be unique) *)
  val name : t -> string

end

(** Build a new namespace of globally unique ids *)
module Make () : S = struct

  type t = int

  (** Fresh variable state *)
  let next_id = ref 0

  (** For storing the variable names *)
  module Id_map = Map.Make (Int)

  (** A global store of variable names. This might not be an ideal approach
      more long-running compilers (it’s a memory leak), but it saves us having
      to cart around the names separately. *)
  let names = ref Id_map.empty

  let fresh name =
    let id = !next_id in
    incr next_id;
    names := Id_map.add id name !names;
    id

  let compare = Int.compare

  let to_int id = id

  let name id = Id_map.find id !names

end
