(** Generative functors for producing new namespaces *)

module Label = struct

  module type S = sig

    type t

    val make : string -> t
    val compare : t -> t -> int
    val to_string : t -> string

  end

  module Make () : S = struct

    type t = string

    let make name = name
    let compare = String.compare
    let to_string name = name

  end

end

module Fresh = struct

  module type S = sig

    type t

    val fresh : string -> t
    val compare : t -> t -> int
    val to_string : t -> string

  end

  module Make () : S = struct

    type t = int * string

    let next_id = ref 0

    let fresh (name : string) : t =
      let id = !next_id in
      incr next_id;
      id, name

    let compare (id1, _) (id2, _) =
      Int.compare id1 id2

    let to_string (id, name) =
      Printf.sprintf "%s%i" name id

  end

end

module Debruijn = struct

  module type S = sig

    module Index : sig

      type t

    end

    module Env : sig

      type 'a t

      val empty : 'a t
      val extend : 'a -> 'a t -> 'a t
      val lookup : Index.t -> 'a t -> 'a
      val find_index : ('a -> bool) -> 'a t -> Index.t option
      val of_seq : 'a Seq.t -> 'a t

    end

  end

  module Make () : S = struct

    module Index = struct

      type t = int

    end

    module Env = struct

      type 'a t = 'a list

      let empty = []
      let extend x env = x :: env
      let lookup i env = List.nth env i
      let find_index = List.find_index
      let of_seq = List.of_seq

    end

  end

end
