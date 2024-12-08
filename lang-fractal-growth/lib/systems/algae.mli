(** {0 Model of the growth of Algae} *)

(** Lindenmayer's original L-system for modelling the growth of algae. *)

(** Alphabet of symbols for this system *)
type symbol

include System.Grammar
  with type symbol := symbol
  (** @open *)


(** {1 String interpretation} *)

(** Convert a symbol to a string *)
val string_of_symbol : symbol -> string

(** Convert a word to a string *)
val string_of_word : symbol list -> string
