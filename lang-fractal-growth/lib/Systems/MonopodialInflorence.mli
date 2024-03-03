(** The development of a {{:https://en.wikipedia.org/wiki/Inflorescence}
    monopodial inflorescence} *)


(** The development of an individual flower *)
module Flower : sig

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

end


(** Alphabet of symbols for this system *)
type symbol

(** Convert a flower to a symbol *)
val symbol_of_flower : Flower.symbol -> symbol

include System.Grammar
  with type symbol := symbol
  (** @open *)


(** {1 String interpretation} *)

(** Convert a symbol to a string *)
val string_of_symbol : symbol -> string

(** Convert a word to a string *)
val string_of_word : symbol list -> string
