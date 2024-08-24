(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** The number of bindings bound in an environment *)
module Size : sig

  type t = private int

  val zero : t
  val succ : t -> t
  val to_int : t -> int

end

(** {i De Bruijn index} that represents a variable occurance by the number of
    binders between the occurance and the binder it refers to. *)
module rec Index : sig

  type t = private int

  val to_level : Size.t -> t -> Level.t
  (** [to_level s i] converts [i] to a {!Level.t} that is bound in an
      environment of the supplied size [s].

      Assumes that [ Size.to_int s > to_int i ].
  *)

  val to_int : t -> int

end

(** A {i De Bruijn level} that represents a variable occurance by the number of
    binders from the top of the environment to the binder that the ocurrance
    refers to.

    These do not change their meaning as new bindings are added to the environment.
*)
and Level : sig

  type t = private int

  val next : Size.t -> Level.t
  (** [next s] is the next level to be bound in an environment of size [s] *)

  val to_index : Size.t -> t -> Index.t
  (** [to_index s l] converts [l] to an {!Index.t} that is bound in an
      environment of the supplied size [s].

      Assumes that [ Size.to_int s > to_int l ].
  *)

  val to_int : t -> int

end

(** An environment of bindings that can be looked up with an {!Index.t}
    directly, or by converting a {!Level.t} to an index with {!Level.to_index}.
*)
module Env : sig

  type 'a t

  val empty : 'a. 'a t
  (** [empty] is an environment with no bindings *)

  val extend : 'a. 'a -> 'a t -> 'a t
  (** [extend x xs] adds a new binding [x] to [xs]. This invalidates indices,
      but levels are preserved *)

  val lookup : 'a. Index.t -> 'a t -> 'a
  val lookup_opt : 'a. Index.t -> 'a t -> 'a option
  val size : 'a. 'a t -> Size.t

end
