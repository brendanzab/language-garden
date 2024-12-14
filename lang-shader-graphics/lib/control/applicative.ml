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

end

module Compose (F : S) (G : S) = Make (struct

  include Functor.Compose (F) (G)

  let pure x = F.pure (G.pure x)
  let apply f x = F.apply (F.map G.apply f) x

end)
