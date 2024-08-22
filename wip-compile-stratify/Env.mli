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
type ('ns, 'entry) t

(** Initial, empty environment *)
val empty : 'ns 'entry. ('ns, 'entry) t

(** Bind a new entry in the environment *)
val bind_entry : 'ns 'entry. 'entry -> ('ns, 'entry) t -> ('ns, 'entry) t

(** Get an entry in the environment using an index *)
val lookup : 'ns 'entry. 'ns index -> ('ns, 'entry) t -> 'entry

(** Return the index of an entry in the environment *)
val entry_index : 'ns 'entry. 'entry -> ('ns, 'entry) t -> 'ns index option


(** {1 Erased environments } *)

(** The size of an environment *)
type 'ns size

(** Return the size of an environment *)
val size : 'ns 'entry. ('ns, 'entry) t -> 'ns size

(** The size of an initial, empty environment *)
val empty_size : 'ns. 'ns size

(** Bind a new level *)
val bind_level : 'ns. 'ns size -> 'ns size

(** Return a level that will point to the entry bound in the environment *)
val next_level : 'ns. 'ns size -> 'ns level

(** Converts a {!level} to an {!index} that is bound in an environment of the
    supplied size. Assumes that [ size > level ]. *)
val level_to_index : 'ns. 'ns size -> 'ns level -> 'ns index
