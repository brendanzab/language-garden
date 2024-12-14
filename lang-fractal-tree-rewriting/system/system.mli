module Deterministic : sig

  module type Core = System_intf.Deterministic.Core
  module type S = System_intf.Deterministic.S

  module Make (X : Core) : S
    with type t = X.t

end
