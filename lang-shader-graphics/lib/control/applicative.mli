module type Core = Applicative_intf.Core
module type S = Applicative_intf.S

module Make (X : Core) : S
  with type 'a t = 'a X.t

(** The composition of two applicative functors *)
module Compose (F : S) (G : S) : S
  with type 'a t = 'a G.t F.t
