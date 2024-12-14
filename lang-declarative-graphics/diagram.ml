(** Declarative diagramming API *)

type style = [`solid | `none]
type vec2 = float * float

module type S = sig

  type t

  val circle : diameter:float -> t
  val line : vec2 -> vec2 -> t

  val stroke : style -> t -> t
  val fill : style -> t -> t

  val stack : t list -> t
  val rotate : radians:float -> t -> t
  val translate : vec2 -> t -> t

end
