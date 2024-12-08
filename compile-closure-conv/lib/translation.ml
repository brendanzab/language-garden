(** {0 Translation passes} *)

(** Alpha renaming translation *)
module Fun_to_fun_a = Translation__fun_to_fun_a

(** Typed closure conversion on nameless terms *)
module Fun_to_clos = Translation__fun_to_clos

(** Typed closure conversion on alpha renamed terms *)
module Fun_a_to_clos_a = Translation__fun_a_to_clos_a

(** Typed lambda lifting on alpha renamed terms *)
module Fun_a_to_lifted_a = Translation__fun_a_to_lifted_a
