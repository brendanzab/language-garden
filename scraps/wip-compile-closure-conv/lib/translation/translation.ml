(** {0 Translation passes} *)

(** Alpha renaming translation *)
module Fun_to_fun_a = Fun_to_fun_a

(** Typed closure conversion on nameless terms *)
module Fun_to_clos = Fun_to_clos

(** Typed closure conversion on alpha renamed terms *)
module Fun_a_to_clos_a = Fun_a_to_clos_a

(** Typed lambda lifting on alpha renamed terms *)
module Fun_a_to_lifted_a = Fun_a_to_lifted_a
