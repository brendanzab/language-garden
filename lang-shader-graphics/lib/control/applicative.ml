include Applicative_intf

module Make (X : Core) : S
  with type 'a t = 'a X.t
= struct

  include X
  include Functor.Make (X)

  module O = struct

    include O

    let ( <*> ) = apply

  end

  open O

  let map0 = pure
  let map1 = map
  let map2 f x1 x2 = map1 f x1 <*> x2
  let map3 f x1 x2 x3 = map2 f x1 x2 <*> x3
  let map4 f x1 x2 x3 x4 = map3 f x1 x2 x3 <*> x4

end

module Compose (F : S) (G : S) = Make (struct

  include Functor.Compose (F) (G)

  let pure x = F.pure (G.pure x)
  let apply f x = F.apply (F.map G.apply f) x

end)
