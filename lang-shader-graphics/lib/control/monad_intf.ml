module type Core = sig

  type 'a t

  include Applicative.Core with type 'a t := 'a t

  val flat_map : 'a 'b. ('a -> 'b t) -> 'a t -> 'b t
  (** Apply a function to a value of type ['a t], flattening the result into the
      monadic structure. *)

end

module type S = sig

  include Core

  include Applicative.S with type 'a t := 'a t

  val flatten : 'a. 'a t t -> 'a t
  (** Remove one level of monadic structure *)

  (** Convenience operators *)
  module O : sig

    include module type of O

    val ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t
    (** Alias for {!flat_map} *)

    val ( and* ) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
    (** Alias for {!both} *)

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
