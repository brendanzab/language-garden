module Deterministic = struct

  module type Core = sig

    type t

    val axiom : t
    val rules : (t -> t) -> t -> t
    val draw : 'd. (module Diagram.S with type t = 'd) -> (t -> 'd) -> t -> 'd

  end

  module type S = sig

    include Core

    val step : t -> t
    val grow : ?axiom:t -> int -> t
    val generations : ?axiom:t -> unit -> t Seq.t
    val render : 'd. (module Diagram.S with type t = 'd) -> t -> 'd

  end

end
