(** Extensions to the {!Stdlib.List} module *)
module List : sig

  include module type of List

  (** Returns a list of duplicate elements in a list *)
  val find_dupes : 'a. 'a list -> 'a list

end
