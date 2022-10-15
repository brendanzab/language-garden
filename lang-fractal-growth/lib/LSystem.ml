(** A symbol in an alphabet *)
module type Symbol = sig

  (** The type of a symbol *)
  type t

  (** Convert the symbol to a string *)
  val to_string : t -> string

end


(** Signature of a deterministic, context-free L-System *)
module type S = sig

  (** Alphabet of symbols for this system *)
  module Symbol : Symbol

  (** The initial state of the system *)
  val axiom : Symbol.t list

  (** Production rules defining how symbols will be replaced *)
  val rules : Symbol.t -> Symbol.t list

end


(** Derived utility functions for working with L-systems *)
module Util (System : S) : sig

  open System

  (** Apply the rewrite rules in parallel *)
  val step : Symbol.t list -> Symbol.t list

  (** Unfold a list of productions *)
  val generate : Symbol.t list -> Symbol.t list Seq.t

  (** Render a word as a string *)
  val string_of_word : Symbol.t list -> string

end = struct

  let step =
    List.concat_map System.rules

  let generate w =
    Seq.unfold (fun w -> Some (w, step w)) w

  let string_of_word w =
    List.map System.Symbol.to_string w
      |> String.concat ""

end
