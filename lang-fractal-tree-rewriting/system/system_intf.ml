open Declarative_graphics_core

module Deterministic = struct

  module type Core = sig

    type t

    val axiom : t
    val rules : (t -> t) -> t -> t
    val draw : (module D : Diagram.S) -> (t -> D.t) -> t -> D.t

  end

  module type S = sig

    include Core

    val step : t -> t
    val grow : ?axiom:t -> int -> t
    val generations : ?axiom:t -> unit -> t Seq.t
    val render : (module D : Diagram.S) -> t -> D.t

  end

end
