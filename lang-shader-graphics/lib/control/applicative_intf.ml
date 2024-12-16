module type Core = sig

  type 'a t

  include Functor.Core with type 'a t := 'a t

  (** Embed a pure value in [t] *)
  val pure : 'a. 'a -> 'a t

  (** Apply a function embedded in [t] to a value embedded in [t] *)
  val apply : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t

end

(** Applicative functors

    This can be seen as a generalisation of the {!Functor.S} signature, but
    where {!Functor.S.map} can take any number of arguments.

    Applicatives can also be viewed as a restricted form of the {!Monad.S} signature,
    where one computation cannot depend on the result of another computation.
    For this reason applicatives are a useful way of describing effects that can be parallelised.
*)
module type S = sig

  include Core

  include Functor.S with type 'a t := 'a t

  val both : 'a 'b. 'a t -> 'b t -> ('a * 'b) t

  val map0 : 'a. 'a -> 'a t
  val map1 : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  val map2 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : 'a 'b 'c 'd. ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : 'a 'b 'c 'd 'e. ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  (** Convenience operators *)
  module O : sig

    include module type of O

    val ( let+ ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t
    (** Alias for {!map} *)

    val ( and+ ) : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
    (** Alias for {!both} *)

    val ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t
    (** Alias for {!apply} *)

  end

end
