(** {0 Model of Anabaena catenula}*)

(** Development of a multicelluar filament in the blue-green bactieria,
    {{: https://en.wikipedia.org/wiki/Anabaena} Anabaena catenula}. *)

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
