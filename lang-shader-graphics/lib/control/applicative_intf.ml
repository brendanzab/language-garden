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

  (** Convenience operators *)
  module O : sig

    include module type of O

    val ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t

  end

end
