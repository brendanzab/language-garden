(** Extensions to the {!Stdlib.List} module *)
module List : sig

  include module type of List

  (** Returns the index of the given element in the list *)
  val elem_index : 'a -> 'a list -> int option

  (** Returns a list of duplicate elements in a list *)
  val find_dupes : 'a list -> 'a list

end
