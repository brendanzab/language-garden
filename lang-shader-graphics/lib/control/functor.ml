include Functor_intf

module Make (X : Core) : S
  with type 'a t = 'a X.t
= struct

  include X

  let void_left : type a b. a -> b t -> a t =
    fun x f -> map (Fun.const x) f

  let void_right : type a b. b t -> a -> a t =
    fun f x -> map (Fun.const x) f

  module O = struct

    let ( let+ ) t f = map f t
    let ( >|= ) t f = map f t
    let ( <$> ) = map
    let ( <$ ) = void_left
    let ( $> ) = void_right

  end

end

module Compose (F : S) (G : S) = Make (struct

  type 'a t = 'a G.t F.t

  let map f x = F.map (G.map f) x

end)
