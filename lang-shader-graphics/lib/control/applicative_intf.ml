module type Core = sig

  type 'a t

  include Functor.Core with type 'a t := 'a t

  (** Embed a pure value in [t] *)
  val pure : 'a. 'a -> 'a t

  (** Apply a function embedded in [t] to a value embedded in [t] *)
  val apply : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t

end

(** Applicative functors *)
module type S = sig

  include Core

  include Functor.S with type 'a t := 'a t

  val map0 : 'a. 'a -> 'a t
  val map1 : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  val map2 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : 'a 'b 'c 'd. ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : 'a 'b 'c 'd 'e. ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

  (** Convenience operators *)
  module O : sig

    include module type of O

    val ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t

  end

end
