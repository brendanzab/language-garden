(** {0 Monadic effects} *)

(** Order-dependent effects *)
module Monad = struct

  module type S = sig

    type 'a m

    val pure : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m

  end


  (** Operators to make working with monads more pleasant *)
  module type Notation = sig

    type 'a m

    (** Binding operators *)

    val (let*) : 'a m -> ('a -> 'b m) -> 'b m
    val (and*) : 'a m -> 'b m -> ('a * 'b) m
    val (let+) : 'a m -> ('a -> 'b) -> 'b m
    val (and+) : 'a m -> 'b m -> ('a * 'b) m

  end


  module Notation (M : S) : Notation

    with type 'a m = 'a M.m

  = struct

    type 'a m = 'a M.m

    let (let*) = M.bind

    let (and*) m n =
      let* x = m in
      let* y = n in
      M.pure (x, y)

    let (let+) m f = M.bind m (fun x -> M.pure (f x))

    let (and+) m n = (and*) m n

  end


  (** Utility functions derived from a monad *)
  module Util (M : S) = struct

    open Notation (M)

    let map f x =
      let* x = x in
      M.pure (f x)

    let map2 f x1 x2 =
      let* x1 = x1 in
      let* x2 = x2 in
      M.pure (f x1 x2)

    let map3 f x1 x2 x3 =
      let* x1 = x1 in
      let* x2 = x2 in
      let* x3 = x3 in
      M.pure (f x1 x2 x3)

    let map4 f x1 x2 x3 x4 =
      let* x1 = x1 in
      let* x2 = x2 in
      let* x3 = x3 in
      let* x4 = x4 in
      M.pure (f x1 x2 x3 x4)

  end


  (** An environment with access to a shared value *)
  module type Reader = sig

    type t

    include S

    (** Access the shared value *)
    val ask : t m

    (** Run a computation with the value *)
    val run : t -> 'a m -> 'a

  end


  (** A reader implemented for functions *)
  module FunctionReader (E : sig type t end) : Reader

    with type t = E.t
    with type 'a m = E.t -> 'a

  = struct

    type t = E.t

    type 'a m = E.t -> 'a

    let bind m f = fun x -> f (m x) x
    let pure x = fun _ -> x

    let ask = fun x -> x
    let run x m = m x

  end


  (** A reader with an abstract implementation *)
  module Reader (E : sig type t end) : Reader with type t = E.t =
    FunctionReader (E)


  (** An environment that can access and update some shared state *)
  module type State = sig

    type t

    include S

    (** Access the shared state from the environment *)
    val get : t m

    (** Replace the shared state of the environment *)
    val put : t -> unit m

    (** Embed a state action in the environment *)
    val embed : (t -> 'a * t) -> 'a m

    (** Run a stateful computation using an initial state.
        This is the inverse of the {!state} function. *)
    val run : 'a m -> t -> 'a * t

  end


  (** A state monad implemented for functions *)
  module FunctionState (E : sig type t end) : State

    with type t = E.t
    with type 'a m = E.t -> 'a * E.t

  = struct

    type t = E.t

    type 'a m = E.t -> 'a * E.t

    let bind m f = fun x ->
      let x', s = m x in
      f x' s

    let pure x = fun s -> x, s

    let get = fun s -> s, s
    let put s = fun _ -> (), s

    let embed f = fun s -> f s
    let run m s = m s

  end


  (** A state monad with an abstract implementation *)
  module State (E : sig type t end) : State with type t = E.t =
    FunctionState (E)

end
