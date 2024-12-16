include Monad_intf

module Make (X : Core) : S
  with type 'a t = 'a X.t
= struct

  include X
  include Applicative.Make (X)

  let join x =
    bind x Fun.id

  module O = struct

    include O

    let ( let* ) = bind
    let ( and* ) = both

  end

end

module Reader = struct

  module type S = Reader.S

  module Function (V : sig type t end) = struct

    type value = V.t

    include Make (struct

      type 'a t = value -> 'a

      let map f x = fun v ->  f (x v)
      let pure v = fun _ -> v
      let apply f x = fun v -> (f v) (x v)
      let bind t f = fun v -> f (t v) v

    end)

    let read = fun v -> v
    let scope f t = fun v -> t (f v)

    let run v t = t v

  end

  module Make = Function

end


module State = struct

  module type S = State.S

  module Function (V : sig type t end) = struct

    type state = V.t

    include Make (struct

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

    end)

    let get = fun s -> s, s
    let put s = fun _ -> (), s

    let embed f = fun s -> f s
    let run x s = x s

  end

  module Make = Function

end

(** Operations on the output (returned value) of a function *)
module Output (Input : sig type t end) = Make (struct

  type 'a t = Input.t -> 'a

  let map f g = fun x -> f (g x)
  let pure x = fun _ -> x
  let apply f g = fun x -> (f x) (g x)
  let bind f g = fun x -> g (f x) x

end)
