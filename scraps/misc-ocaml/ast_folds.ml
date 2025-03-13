(** Folding over an AST using recursion schemes (a response to
    https://bsky.app/profile/bandukwala.me/post/3liwrjfotek2a).
*)

[@@@warning "-unused-value-declaration"]

module Expr = struct

  module Layer = struct

    type 'a t =
      | Mul of 'a * 'a
      | Add of 'a * 'a
      | Int of int

    let map (f : 'a -> 'b) (layer : 'a t) : 'b t =
      match layer with
      | Mul (x, y) -> Mul (f x, f y)
      | Add (x, y) -> Add (f x, f y)
      | Int x -> Int x

  end

  type t = {
    layer : t Layer.t;
  }

  (* See https://hackage.haskell.org/package/recursion-schemes-5.2.3/docs/Data-Functor-Foldable.html#v:fold *)
  let rec fold (f : 'a Layer.t -> 'a) (expr : t) : 'a =
    f (Layer.map (fold f) expr.layer)

end
