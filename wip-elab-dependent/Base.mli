(** Extended {!List} module *)
module List : sig

  include module type of List

  (** Returns the index of the given element in the list *)
  val elem_index : 'a -> 'a t -> int option

end
