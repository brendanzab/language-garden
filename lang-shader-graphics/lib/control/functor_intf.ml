module type Core = sig

  type 'a t

  val map : 'a 'b. ('a -> 'b) -> ('a t -> 'b t)
  (** Turn a function of type ['a -> 'b] into a function of type
      ['a t -> 'b t]. *)

  (** {1 Laws}

      - Identity: [map Fun.id = Fun.id]
      - Composition: [map (f >> g) = map f >> map g]
  *)

end

(** A type constructor ['a t] that supports a mapping operation [map]. *)
module type S = sig
  (** This is technically an “endofunctor” between the ‘category’ of OCaml
      types and functions. *)

  include Core

  val void_left : 'a 'b. 'a -> 'b t -> 'a t
  val void_right : 'a 'b. 'b t -> 'a -> 'a t

  (** Convenience operators *)
  module O : sig

    val ( let+ ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t
    val ( >|= ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t
    val ( <$> ) : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
    val ( <$ ) : 'a 'b. 'a -> 'b t -> 'a t
    val ( $> ) : 'a 'b. 'b t -> 'a -> 'a t

  end

end
