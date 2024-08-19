module Functor = struct

  (** A type constructor ['a t] that supports a mapping operation [map]. *)
  module type S = sig
    (** This is technically an “endofunctor” between the ‘category’ of OCaml
        types and functions. *)

    type 'a t

    (** Turn a function of type ['a -> 'b] into a function of type
        ['a t -> 'b t]. *)
    val map : ('a -> 'b) -> ('a t -> 'b t)

    (** {1 Laws}

        - Identity: [map Fun.id = Fun.id]
        - Composition: [map (f >> g) = map f >> map g]
    *)

  end


  (** Utility functions derived from functors *)
  module Util (F : S) = struct

    open F

    let void_left : type a b. a -> b t -> a t =
      fun x f -> map (Fun.const x) f

    let void_right : type a b. b t -> a -> a t =
      fun f x -> map (Fun.const x) f

  end


  (** Operators to make working with functors more pleasant *)
  module type Notation = sig

    type 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <$ ) : 'a -> 'b t -> 'a t
    val ( $> ) : 'b t -> 'a -> 'a t

  end


  (** Construct some notation for a functor *)
  module Notation (F : S) : Notation

    with type 'a t = 'a F.t

  = struct

    open F
    open Util (F)

    type 'a t = 'a F.t

    let ( let+ ) t f = map f t
    let ( >|= ) t f = map f t
    let ( <$> ) = map
    let ( <$ ) = void_left
    let ( $> ) = void_right

  end


  (** The composition of two functors *)
  module Compose (F : S) (G : S) : S

    with type 'a t = 'a G.t F.t

  = struct

    type 'a t = 'a G.t F.t

    let map f x = F.map (G.map f) x

  end

end


(** Applicative functors *)
module Applicative = struct

  module type S = sig

    type 'a t

    include Functor.S with type 'a t := 'a t

    (** Embed a pure value in [t] *)
    val pure : 'a -> 'a t

    (** Apply a function embedded in [t] to a value embedded in [t] *)
    val apply : ('a -> 'b) t -> 'a t -> 'b t

  end


  (** Operators to make working with applicatives more pleasant *)
  module type Notation = sig

    type 'a t

    include Functor.Notation with type 'a t := 'a t

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  end


  (** Construct some notation for an applicative *)
  module Notation (A : S) : Notation

    with type 'a t = 'a A.t

  = struct

    include Functor.Notation (A)

    let ( <*> ) = A.apply

  end


  (** The composition of two applicatives *)
  module Compose (F : S) (G : S) : S

    with type 'a t = 'a G.t F.t

  = struct

    include Functor.Compose (F) (G)

    let pure x = F.pure (G.pure x)
    let apply f x = F.apply (F.map G.apply f) x

  end

end


module Monad = struct

  module type S = sig

    type 'a t

    include Applicative.S with type 'a t := 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

  end


  (** Operators to make working with monads more pleasant *)
  module type Notation = sig

    type 'a t

    include Applicative.Notation with type 'a t := 'a t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  end


  (** Construct some notation for a monad *)
  module Notation (M : S) : Notation

    with type 'a t = 'a M.t

  = struct

    include Applicative.Notation (M)

    let ( let* ) = M.bind

    let ( and* ) t n =
      let* x = t in
      let* y = n in
      M.pure (x, y)

    let ( and+ ) t n = ( and* ) t n

  end


  (** Utility functions derived from a monad *)
  module Util (M : S) = struct

    open Notation (M)

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

    type value

    include S

    (** Access the shared value *)
    val read : value t

    (** Execute a computation in an environment that has been altered by a function *)
    val scope : (value -> value) -> 'a t -> 'a t

    (** Run a computation with the value *)
    val run : value -> 'a t -> 'a

  end


  (** A reader implemented for functions *)
  module FunctionReader (V : sig type t end) : Reader

    with type value = V.t
    with type 'a t = V.t -> 'a

  = struct

    type value = V.t

    type 'a t = value -> 'a

    let map f x = fun v ->  f (x v)
    let pure v = fun _ -> v
    let apply f x = fun v -> (f v) (x v)
    let bind t f = fun v -> f (t v) v

    let read = fun v -> v
    let scope f t = fun v -> t (f v)

    let run v t = t v

  end


  (** A reader with an abstract implementation *)
  module Reader (V : sig type t end) : Reader with type value = V.t =
    FunctionReader (V)


  (** An environment that can access and update some shared state *)
  module type State = sig

    type state

    include S

    (** Access the shared state from the environment *)
    val get : state t

    (** Replace the shared state of the environment *)
    val put : state -> unit t

    (** Embed a state action in the environment *)
    val embed : (state -> 'a * state) -> 'a t

    (** Run a stateful computation using an initial state.
        This is the inverse of the {!state} function. *)
    val run : 'a t -> state -> 'a * state

  end


  (** A state monad implemented for functions *)
  module FunctionState (S : sig type t end) : State

    with type state = S.t
    with type 'a t = S.t -> 'a * S.t

  = struct

    type state = S.t

    type 'a t = state -> 'a * state

    let map f x = fun s ->
      let x, s = x s in
      f x, s

    let pure x =
      fun s -> x, s

    let apply f x = fun s ->
      let f, s = f s in
      let x, s = x s in
      f x, s

    let bind x f = fun s ->
      let x, s = x s in
      f x s

    let get = fun s -> s, s
    let put s = fun _ -> (), s

    let embed f = fun s -> f s
    let run x s = x s

  end


  (** A state monad with an abstract implementation *)
  module State (S : sig type t end) : State with type state = S.t =
    FunctionState (S)

end


(** Operations on the output (returned value) of a function *)
module Output (Input : sig type t end) : Monad.S

  with type 'a t = Input.t -> 'a

= struct

  type 'a t = Input.t -> 'a

  let map f g = fun x -> f (g x)
  let pure x = fun _ -> x
  let apply f g = fun x -> (f x) (g x)
  let bind f g = fun x -> g (f x) x

end
