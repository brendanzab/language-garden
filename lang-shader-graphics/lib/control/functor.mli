module type Core = Functor_intf.Core
module type S = Functor_intf.S

module Make (X : Core) : S
  with type 'a t = 'a X.t

(** The composition of two functors *)
module Compose (F : S) (G : S) : S
  with type 'a t = 'a G.t F.t
