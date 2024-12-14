module type Core = Monad_intf.Core
module type S = Monad_intf.S

module Make (X : Core) : S
  with type 'a t = 'a X.t

module Reader : sig

  module type S = Monad_intf.Reader.S

  (** A reader implemented for functions *)
  module Function (V : sig type t end) : S
    with type value = V.t
    with type 'a t = V.t -> 'a

  (** A reader with an abstract implementation *)
  module Make (V : sig type t end) : S
    with type value = V.t

end

module State : sig

  module type S = Monad_intf.State.S

  (** A state monad implemented for functions *)
  module Function (V : sig type t end) : S
    with type state = V.t
    with type 'a t = V.t -> 'a * V.t

  (** A state monad with an abstract implementation *)
  module Make (V : sig type t end) : S
    with type state = V.t

end

(** Operations on the output (returned value) of a function *)
module Output (Input : sig type t end) : S
  with type 'a t = Input.t -> 'a
