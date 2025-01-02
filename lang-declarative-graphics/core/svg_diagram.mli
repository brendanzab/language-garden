type t

include Diagram.S with type t := t

val run : view_box:(float * float * float * float) -> t -> string
