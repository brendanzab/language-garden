(** Signature of a deterministic, context-free L-System *)
module type Grammar = sig

  (** Alphabet of symbols for this system *)
  type symbol

  (** The initial state of the system *)
  val axiom : symbol list

  (** Production rules defining how symbols will be replaced *)
  val rules : symbol -> symbol list

end


(** Derived utility functions for working with L-systems *)
module Util (G : Grammar) : sig

  (** Apply the rewrite rules in parallel *)
  val step : G.symbol list -> G.symbol list

  (** Unfold a list of productions *)
  val generate : G.symbol list -> G.symbol list Seq.t

end = struct

  let step =
    List.concat_map G.rules

  let generate w =
    Seq.unfold (fun w -> Some (w, step w)) w

end
