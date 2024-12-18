type style = [`solid | `none]
type vec2 = float * float

module type Core = sig

  type t

  val over : t -> t -> t

  val empty : t
  val circle : diameter:float -> t
  val line : vec2 -> vec2 -> t

  val stroke : style -> t -> t
  val fill : style -> t -> t

  val rotate : radians:float -> t -> t
  val translate : vec2 -> t -> t
  val scale : float -> t -> t

end

module type S = sig

  include Core

  val stack : t list -> t

  val translate_x : float -> t -> t
  val translate_y : float -> t -> t

end
