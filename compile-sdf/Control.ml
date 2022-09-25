(** {0 Monadic effects} *)

(** Order-dependent effects *)
module Monad = struct

  module type S = sig

    type 'a m

    val pure : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m

  end


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
  module Reader (E : sig type t end) : sig

    include S with type 'a m = E.t -> 'a

    (** Access the shared value *)
    val ask : E.t m

    (** Run a computation with the value *)
    val run : E.t -> 'a m -> 'a

  end = struct

    type 'a m = E.t -> 'a

    let bind m f = fun uv -> f (m uv) uv
    let pure x = fun _ -> x

    let ask = fun uv -> uv
    let run uv m = m uv

  end

end
