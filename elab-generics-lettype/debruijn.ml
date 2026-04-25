(** Namespaces of variable occurrences, represented internally as relative
    distances to binders (as opposed to names). This makes capture-avoiding
    substitution and alpha-equivalence easier to implement. *)

(** Output signature of the functor {!Make}. *)
module type S = sig

  (** {i De Bruijn indices} that represent variable occurrences by the number of
      binders between the occurrence and the binder it refers to. *)
  module Index : sig

    type t

    val last : t
    val prev : t -> t

  end

  (** {i De Bruijn levels} that represent variable occurrences by the number of
      binders from the top of the environment to the binder that the occurrence
      refers to. These do not change their meaning as new bindings are added to
      the environment. *)
  module Level : sig

    type t

    val first : t
    val next : t -> t

  end

  (** [level_to_index size level] converts [level] to an index that is bound in
      an environment of the supplied [size], where [size] represents the next
      fresh level to be bound in the environment.

      Assumes that [size > level]. *)
  val level_to_index : Level.t -> Level.t -> Index.t

  (** An environment of bindings that can be looked up directly using a index,
      or by converting to a {!level} using {!level_to_index}. *)
  module Env : sig

    type _ t

    val empty : 'a t
    val extend : 'a -> 'a t -> 'a t

    (** Lookup an entry in the environment using an index *)
    val lookup : Index.t -> 'a t -> 'a

    (** Get the index of the first entry to match the supplied predicate *)
    val find_index : ('a -> bool) -> 'a t -> Index.t option

    (** The level of the next entry to be added to the environment *)
    val size : 'a t -> Level.t

  end

end

(** A {e generative functor} that creates a new namespace of variable bindings.
    Because we have two environments in the core language - i.e. one for types
    and one for terms - this prevents us from mixing up variables from
    different namespaces.

    See {{: https://ocaml.org/manual/5.4/generativefunctors.html} the OCaml
    manual} in the OCaml manual for more information on generative functors. *)
module Make () : S = struct

  module Index = struct

    type t = int

    let last = Int.zero
    let prev = Int.succ

  end

  module Level = struct

    type t = int

    let first = Int.zero
    let next = Int.succ

  end

  let level_to_index size level =
    size - level - 1

  module Env = struct

    type 'a t = 'a list

    let empty = []
    let extend = List.cons
    let lookup index env = List.nth env index
    let find_index = List.find_index
    let size = List.length

  end

end
