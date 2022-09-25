(** Language of linear algebra expressions *)

open MathTypes


(** A language of linear algebra for a GLSL-like shader language *)
module type S = sig

  type 'a repr

  (** Literals *)

  val float : float -> float repr

  val vec2 : float repr -> float repr -> (float vec2) repr
  val vec3 : float repr -> float repr -> float repr -> (float vec3) repr
  val vec4 : float repr -> float repr -> float repr -> float repr -> (float vec4) repr

  val mat2 : (float vec2) repr -> (float vec2) repr -> (float mat2) repr
  val mat3 : (float vec3) repr -> (float vec3) repr -> (float vec3) repr -> (float mat3) repr
  val mat4 : (float vec4) repr -> (float vec4) repr -> (float vec4) repr -> (float vec4) repr -> (float mat4) repr

  (** Functions *)

  val neg : float repr -> float repr
  val neg_v : ('n vecf) repr -> ('n vecf) repr

  val add : float repr -> float repr -> float repr
  val add_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val add_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val sub : float repr -> float repr -> float repr
  val sub_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val sub_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val mul : float repr -> float repr -> float repr
  val mul_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mul_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val div : float repr -> float repr -> float repr
  val div_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val div_vs : ('n vecf) repr -> float repr -> ('n vecf) repr


  val abs : float repr -> float repr
  val abs_v : ('n vecf) repr -> ('n vecf) repr

  val clamp : float repr -> min:float repr -> max:float repr -> float repr
  val clamp_v : ('n vecf) repr -> min:('n vecf) repr -> max:('n vecf) repr -> ('n vecf) repr
  val clamp_vs : ('n vecf) repr -> min:float repr -> max:float repr -> ('n vecf) repr

  val length : ('n vecf) repr -> float repr

  val lerp : float repr -> float repr -> float repr -> float repr
  val lerp_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val lerp_vs : ('n vecf) repr -> ('n vecf) repr -> float repr -> ('n vecf) repr

  val max : float repr -> float repr -> float repr

  val min : float repr -> float repr -> float repr

  val mod_ : float repr -> float repr -> float repr
  val mod_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mod_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val round : float repr -> float repr
  val round_v : ('n vecf) repr -> ('n vecf) repr

  val step : float repr -> float repr -> float repr
  val step_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val step_vs : float repr -> ('n vecf) repr -> ('n vecf) repr

  (** Vector projections *)

  val x : ((float, 'n) vec1n) repr -> float repr
  val y : ((float, 'n) vec2n) repr -> float repr
  val z : ((float, 'n) vec3n) repr -> float repr
  val w : ((float, 'n) vec4n) repr -> float repr

  (** Vector swizzling *)

  val xx : ((float, 'n) vec1n) repr -> (float vec2) repr
  val xy : ((float, 'n) vec2n) repr -> (float vec2) repr
  val xz : ((float, 'n) vec3n) repr -> (float vec2) repr
  val xw : ((float, 'n) vec4n) repr -> (float vec2) repr

  val yx : ((float, 'n) vec2n) repr -> (float vec2) repr
  val yy : ((float, 'n) vec2n) repr -> (float vec2) repr
  val yz : ((float, 'n) vec3n) repr -> (float vec2) repr
  val yw : ((float, 'n) vec4n) repr -> (float vec2) repr

  val zx : ((float, 'n) vec3n) repr -> (float vec2) repr
  val zy : ((float, 'n) vec3n) repr -> (float vec2) repr
  val zz : ((float, 'n) vec3n) repr -> (float vec2) repr
  val zw : ((float, 'n) vec4n) repr -> (float vec2) repr

  val wx : ((float, 'n) vec4n) repr -> (float vec2) repr
  val wy : ((float, 'n) vec4n) repr -> (float vec2) repr
  val wz : ((float, 'n) vec4n) repr -> (float vec2) repr
  val ww : ((float, 'n) vec4n) repr -> (float vec2) repr

  (* TODO: More swizzles *)

end


module type Notation = sig

  type 'a repr

  (* NOTE: These operators are a bit janky... really wish we have modular
           implicits in OCaml. :'( *)

  val (+) : float repr -> float repr -> float repr
  val (|+|) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|+) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val (-) : float repr -> float repr -> float repr
  val (|-|) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|-) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val ( * ) : float repr -> float repr -> float repr
  val (|*|) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|*) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val (/) : float repr -> float repr -> float repr
  val (|/|) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|/) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val (%) : float repr -> float repr -> float repr
  val (|%|) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|%) : ('n vecf) repr -> float repr -> ('n vecf) repr

end


module Notation (M : S) : Notation

  with type 'a repr = 'a M.repr

= struct

  type 'a repr = 'a M.repr

  let (+) = M.add
  let (|+|) = M.add_v
  let (|+) = M.add_vs

  let (-) = M.sub
  let (|-|) = M.sub_v
  let (|-) = M.sub_vs

  let ( * ) = M.mul
  let (|*|) = M.mul_v
  let (|*) = M.mul_vs

  let (/) = M.div
  let (|/|) = M.div_v
  let (|/) = M.div_vs

  let (%) = M.mod_
  let (|%|) = M.mod_v
  let (|%) = M.mod_vs

end
