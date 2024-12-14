module type Core = sig

  type 'a t

  include Applicative.Core with type 'a t := 'a t

  val bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t

end

(** Applicative functors *)
module type S = sig
  (** This is technically an “endofunctor” between the ‘category’ of OCaml
      types and functions. *)

  include Core

  include Applicative.S with type 'a t := 'a t

  (** Convenience operators *)
  module O : sig

    include module type of O

    val ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
    val ( and+ ) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t

  end

end

module Reader = struct

  (** An environment with access to a shared value *)
  module type S = sig

    type value

    include S

    (** Access the shared value *)
    val read : value t

    (** Execute a computation in an environment that has been altered by a function *)
    val scope : 'a. (value -> value) -> 'a t -> 'a t

    (** Run a computation with the value *)
    val run : 'a. value -> 'a t -> 'a

  end

end

module State = struct

  (** An environment that can access and update some shared state *)
  module type S = sig

    type state

    include S

    (** Access the shared state from the environment *)
    val get : state t

    (** Replace the shared state of the environment *)
    val put : state -> unit t

    (** Embed a state action in the environment *)
    val embed : 'a. (state -> 'a * state) -> 'a t

    (** Run a stateful computation using an initial state.
        This is the inverse of the {!state} function. *)
    val run : 'a. 'a t -> state -> 'a * state

  end

end
