(** Language of linear algebra expressions *)

open MathTypes


(** A language of linear algebra for a GLSL-like shader language *)
module type S = sig

  (** Abstract representation of shader expressions *)
  type 'a repr


  (** {1 Literals} *)

  val float : float -> float repr

  val vec2 : float repr -> float repr -> (float vec2) repr
  val vec3 : float repr -> float repr -> float repr -> (float vec3) repr
  val vec4 : float repr -> float repr -> float repr -> float repr -> (float vec4) repr

  val mat2 : (float vec2) repr -> (float vec2) repr -> (float mat2) repr
  val mat3 : (float vec3) repr -> (float vec3) repr -> (float vec3) repr -> (float mat3) repr
  val mat4 : (float vec4) repr -> (float vec4) repr -> (float vec4) repr -> (float vec4) repr -> (float mat4) repr


  (** {1 Arithemetic operators} *)

  val neg : float repr -> float repr
  val neg_vec : ('n vecf) repr -> ('n vecf) repr

  val add : float repr -> float repr -> float repr
  val add_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val add_scalar : ('n vecf) repr -> float repr -> ('n vecf) repr

  val sub : float repr -> float repr -> float repr
  val sub_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val sub_scalar : ('n vecf) repr -> float repr -> ('n vecf) repr

  val mul : float repr -> float repr -> float repr
  val mul_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mul_scalar : ('n vecf) repr -> float repr -> ('n vecf) repr

  val div : float repr -> float repr -> float repr
  val div_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val div_scalar : ('n vecf) repr -> float repr -> ('n vecf) repr

  val mod_ : float repr -> float repr -> float repr
  val mod_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mod_scalar : ('n vecf) repr -> float repr -> ('n vecf) repr


  (** {1 Other functions on scalars and vectors} *)

  val abs : float repr -> float repr
  val abs_vec : ('n vecf) repr -> ('n vecf) repr

  val clamp : float repr -> min:float repr -> max:float repr -> float repr
  val clamp_vec : ('n vecf) repr -> min:('n vecf) repr -> max:('n vecf) repr -> ('n vecf) repr
  val clamp_scalar : ('n vecf) repr -> min:float repr -> max:float repr -> ('n vecf) repr

  val cos : float repr -> float repr
  val cos_vec : ('n vecf) repr -> ('n vecf) repr

  val dot : ('n vecf) repr -> ('n vecf) repr -> float repr

  val length : ('n vecf) repr -> float repr

  val lerp : float repr -> float repr -> float repr -> float repr
  val lerp_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val lerp_scalar : ('n vecf) repr -> ('n vecf) repr -> float repr -> ('n vecf) repr

  val max : float repr -> float repr -> float repr
  val max_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val min : float repr -> float repr -> float repr
  val min_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val round : float repr -> float repr
  val round_vec : ('n vecf) repr -> ('n vecf) repr

  val sin : float repr -> float repr
  val sin_vec : ('n vecf) repr -> ('n vecf) repr

  val sqrt : float repr -> float repr
  val sqrt_vec : ('n vecf) repr -> ('n vecf) repr

  val step : float repr -> float repr -> float repr
  val step_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val step_scalar : float repr -> ('n vecf) repr -> ('n vecf) repr

  val tan : float repr -> float repr
  val tan_vec : ('n vecf) repr -> ('n vecf) repr


  (** {1 Vector component operations} *)

  (** {2 Component projections} *)

  val x : ((float, 'n) vec1n) repr -> float repr
  val y : ((float, 'n) vec2n) repr -> float repr
  val z : ((float, 'n) vec3n) repr -> float repr

  (** {2 Component updates} *)

  val set_x : float repr -> ((float, 'n) vec1n) repr -> ((float, 'n) vec1n) repr
  val set_y : float repr -> ((float, 'n) vec2n) repr -> ((float, 'n) vec2n) repr
  val set_z : float repr -> ((float, 'n) vec3n) repr -> ((float, 'n) vec3n) repr
  val set_w : float repr -> ((float, 'n) vec4n) repr -> ((float, 'n) vec4n) repr

  (** {2 Swizzle operators} *)

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

  val xxx : ((float, 'n) vec1n) repr -> (float vec3) repr
  val xxy : ((float, 'n) vec2n) repr -> (float vec3) repr
  val xxz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val xxw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val xyx : ((float, 'n) vec2n) repr -> (float vec3) repr
  val xyy : ((float, 'n) vec2n) repr -> (float vec3) repr
  val xyz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val xyw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val xzx : ((float, 'n) vec3n) repr -> (float vec3) repr
  val xzy : ((float, 'n) vec3n) repr -> (float vec3) repr
  val xzz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val xzw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val xwx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val xwy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val xwz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val xww : ((float, 'n) vec4n) repr -> (float vec3) repr

  val yxx : ((float, 'n) vec2n) repr -> (float vec3) repr
  val yxy : ((float, 'n) vec2n) repr -> (float vec3) repr
  val yxz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val yxw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val yyx : ((float, 'n) vec2n) repr -> (float vec3) repr
  val yyy : ((float, 'n) vec2n) repr -> (float vec3) repr
  val yyz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val yyw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val yzx : ((float, 'n) vec3n) repr -> (float vec3) repr
  val yzy : ((float, 'n) vec3n) repr -> (float vec3) repr
  val yzz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val yzw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val ywx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val ywy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val ywz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val yww : ((float, 'n) vec4n) repr -> (float vec3) repr

  val zxx : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zxy : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zxz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zxw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val zyx : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zyy : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zyz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zyw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val zzx : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zzy : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zzz : ((float, 'n) vec3n) repr -> (float vec3) repr
  val zzw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val zwx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val zwy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val zwz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val zww : ((float, 'n) vec4n) repr -> (float vec3) repr

  val wxx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wxy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wxz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wxw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wyx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wyy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wyz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wyw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wzx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wzy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wzz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wzw : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wwx : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wwy : ((float, 'n) vec4n) repr -> (float vec3) repr
  val wwz : ((float, 'n) vec4n) repr -> (float vec3) repr
  val www : ((float, 'n) vec4n) repr -> (float vec3) repr

  (* TODO: More swizzle operators *)

end


module type Notation = sig

  type 'a repr

  (* NOTE: These operators are a bit janky... really wish we had modular
           implicits in OCaml. :'(

    The idea is that the vertical pipes are used when the argument is a vector.
    This might end up getting weird as we add matrix operations as well!
  *)

  val (!!) : float -> float repr

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

  let (!!) = M.float

  let (+) = M.add
  let (|+|) = M.add_vec
  let (|+) = M.add_scalar

  let (-) = M.sub
  let (|-|) = M.sub_vec
  let (|-) = M.sub_scalar

  let ( * ) = M.mul
  let (|*|) = M.mul_vec
  let (|*) = M.mul_scalar

  let (/) = M.div
  let (|/|) = M.div_vec
  let (|/) = M.div_scalar

  let (%) = M.mod_
  let (|%|) = M.mod_vec
  let (|%) = M.mod_scalar

end
