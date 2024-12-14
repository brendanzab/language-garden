(** Parallel tree rewriting systems *)

open Declarative_graphics

module Deterministic = struct

  module type Core = System_intf.Deterministic.Core
  module type S = System_intf.Deterministic.S

  module Make (X : Core) : S
    with type t = X.t
  = struct

    include X

    let rec step tree =
      rules step tree

    let grow ?(axiom = axiom) (iters : int) : t =
      let rec go iters tree =
        if iters < 0 then tree else
          (go [@tailcall]) (iters - 1) (step tree)
      in
      go iters axiom

    let generations ?(axiom = axiom) () =
      Seq.unfold (fun t -> Some (t, step t)) axiom

    let render (type d) (module D : Diagram.S with type t = d) : t -> d =
      let draw = draw (module D) in
      let rec render (tree : t) : d =
        draw render tree
      in
      render

  end

end
