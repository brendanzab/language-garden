(** {0 Environments} *)

(** Environments parameterised by a phantom namespace parameter. *)


(** {1 Nameless variables} *)

(** {i De Bruijn index} that represents a variable by the number of binders
    between the variable and the binder it refers to. *)
type 'ns index

(** {i De Bruijn level} that represents a variable by the number of binders
    from the top of the environment to the binder that it refers to. These do
    not change their meaning as new bindings are added to the environment. *)
type 'ns level


(** {1 Environments of entries} *)

(** An environment of bindings that can be looked up directly using a
    {!index}, or by inverting a {!level} using {!level_to_index}. *)
type ('ns, 'a) t

(** Bind a new entry in the environment *)
val bind_entry : 'a -> ('ns, 'a) t -> ('ns, 'a) t

(** Get an entry in the environment using an index *)
val get_index : 'ns index -> ('ns, 'a) t -> 'a


(** {1 Erased environments } *)

(** The size of an environment *)
type 'ns size

(** Converts a {!level} to an {!index} that is bound in an environment of the
    supplied size. Assumes that [ size > level ]. *)
val level_to_index : 'ns size -> 'ns level -> 'ns index

(** Bind a new level *)
val bind_level : 'ns size -> 'ns size

(** Return a level that will point to the entry bound in the environment *)
val next_level : 'ns size -> 'ns level
