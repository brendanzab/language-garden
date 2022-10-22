(** Quadratic {{:https://en.wikipedia.org/wiki/Koch_snowflake} Koch island} *)

(** Alphabet of symbols for this system *)
type symbol

include FractalGrowth.LSystem.Grammar
  with type symbol := symbol
  (** @open *)


(** {1 String interpretation} *)

(** Convert a symbol to a string *)
val string_of_symbol : symbol -> string

(** Convert a word to a string *)
val string_of_word : symbol list -> string
