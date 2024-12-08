(** Production systems *)

module type Core = sig

  type t

  val axiom : t
  val rules : (t -> t) -> t -> t
  val draw : 'd. (module Diagram.S with type t = 'd) -> (t -> 'd) -> t -> 'd

end

module type S = sig

  include Core

  val step : t -> t
  val grow : ?axiom:t -> int -> t
  val generations : ?axiom:t -> unit -> t Seq.t
  val render : 'd. (module Diagram.S with type t = 'd) -> t -> 'd

end

module Make (X : Core) : S with type t = X.t = struct

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

module Binary_tree = Make (Binary_tree)
