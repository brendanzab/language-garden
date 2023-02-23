(** {0 Translation passes} *)

(** Alpha renaming translation *)
module FunToFunA = Translation_FunToFunA

(** Typed closure conversion on nameless terms *)
module FunToClos = Translation_FunToClos

(** Typed closure conversion on alpha renamed terms *)
module FunAToClosA = Translation_FunAToClosA
