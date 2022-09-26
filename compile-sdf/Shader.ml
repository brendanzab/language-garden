(** {1 Typed shader language expressions *)

open ShaderTypes


(** A shader language that supports a variety of operators and functions related
    to linear algebra *)
module type S = sig

  (** Abstract representation of shader expressions *)
  type 'a repr


  (** {1 Literals} *)

  val float : float -> vec1f repr

  (* NOTE: The return type of [vec1f repr] for the [float] function is a bit
     strange, but it allows for nicer overloads in the following functions.
     It would be nice to figure out a better way to model this however. *)

  val vec2 : vec1f repr -> vec1f repr -> vec2f repr
  val vec3 : vec1f repr -> vec1f repr -> vec1f repr -> vec3f repr
  val vec4 : vec1f repr -> vec1f repr -> vec1f repr -> vec1f repr -> vec4f repr

  val mat2 : vec2f repr -> vec2f repr -> mat2f repr
  val mat3 : vec3f repr -> vec3f repr -> vec3f repr -> mat3f repr
  val mat4 : vec4f repr -> vec4f repr -> vec4f repr -> vec4f repr -> mat4f repr


  (** {1 Arithemetic operators} *)

  val neg : ('n vecf) repr -> ('n vecf) repr

  val add : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val add_scalar : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val sub : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val sub_scalar : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val mul : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mul_scalar : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val div : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val div_scalar : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val mod_ : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mod_scalar : ('n vecf) repr -> vec1f repr -> ('n vecf) repr


  (** {1 Other functions on scalars and vectors} *)

  val abs : ('n vecf) repr -> ('n vecf) repr

  val clamp : ('n vecf) repr -> min:('n vecf) repr -> max:('n vecf) repr -> ('n vecf) repr
  val clamp_scalar : ('n vecf) repr -> min:vec1f repr -> max:vec1f repr -> ('n vecf) repr

  val cos : ('n vecf) repr -> ('n vecf) repr

  val dot : ('n vecf) repr -> ('n vecf) repr -> vec1f repr

  val length : ('n vecf) repr -> vec1f repr

  val lerp : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val lerp_scalar : ('n vecf) repr -> ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val max : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val min : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val round : ('n vecf) repr -> ('n vecf) repr

  val sin : ('n vecf) repr -> ('n vecf) repr

  val sqrt : ('n vecf) repr -> ('n vecf) repr

  val step : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val step_scalar : vec1f repr -> ('n vecf) repr -> ('n vecf) repr

  val tan : ('n vecf) repr -> ('n vecf) repr


  (** {1 Vector component operations} *)

  (** {2 Component projections} *)

  val x : ('n vec_ge1f) repr -> vec1f repr
  val y : ('n vec_ge2f) repr -> vec1f repr
  val z : ('n vec_ge3f) repr -> vec1f repr

  (** {2 Component updates} *)

  val set_x : vec1f repr -> ('n vec_ge1f) repr -> ('n vec_ge1f) repr
  val set_y : vec1f repr -> ('n vec_ge2f) repr -> ('n vec_ge2f) repr
  val set_z : vec1f repr -> ('n vec_ge3f) repr -> ('n vec_ge3f) repr
  val set_w : vec1f repr -> ('n vec_ge4f) repr -> ('n vec_ge4f) repr

  (** {2 Swizzle operators} *)

  val xx : ('n vec_ge1f) repr -> vec2f repr
  val xy : ('n vec_ge2f) repr -> vec2f repr
  val xz : ('n vec_ge3f) repr -> vec2f repr
  val xw : ('n vec_ge4f) repr -> vec2f repr

  val yx : ('n vec_ge2f) repr -> vec2f repr
  val yy : ('n vec_ge2f) repr -> vec2f repr
  val yz : ('n vec_ge3f) repr -> vec2f repr
  val yw : ('n vec_ge4f) repr -> vec2f repr

  val zx : ('n vec_ge3f) repr -> vec2f repr
  val zy : ('n vec_ge3f) repr -> vec2f repr
  val zz : ('n vec_ge3f) repr -> vec2f repr
  val zw : ('n vec_ge4f) repr -> vec2f repr

  val wx : ('n vec_ge4f) repr -> vec2f repr
  val wy : ('n vec_ge4f) repr -> vec2f repr
  val wz : ('n vec_ge4f) repr -> vec2f repr
  val ww : ('n vec_ge4f) repr -> vec2f repr

  val xxx : ('n vec_ge1f) repr -> vec3f repr
  val xxy : ('n vec_ge2f) repr -> vec3f repr
  val xxz : ('n vec_ge3f) repr -> vec3f repr
  val xxw : ('n vec_ge4f) repr -> vec3f repr
  val xyx : ('n vec_ge2f) repr -> vec3f repr
  val xyy : ('n vec_ge2f) repr -> vec3f repr
  val xyz : ('n vec_ge3f) repr -> vec3f repr
  val xyw : ('n vec_ge4f) repr -> vec3f repr
  val xzx : ('n vec_ge3f) repr -> vec3f repr
  val xzy : ('n vec_ge3f) repr -> vec3f repr
  val xzz : ('n vec_ge3f) repr -> vec3f repr
  val xzw : ('n vec_ge4f) repr -> vec3f repr
  val xwx : ('n vec_ge4f) repr -> vec3f repr
  val xwy : ('n vec_ge4f) repr -> vec3f repr
  val xwz : ('n vec_ge4f) repr -> vec3f repr
  val xww : ('n vec_ge4f) repr -> vec3f repr

  val yxx : ('n vec_ge2f) repr -> vec3f repr
  val yxy : ('n vec_ge2f) repr -> vec3f repr
  val yxz : ('n vec_ge3f) repr -> vec3f repr
  val yxw : ('n vec_ge4f) repr -> vec3f repr
  val yyx : ('n vec_ge2f) repr -> vec3f repr
  val yyy : ('n vec_ge2f) repr -> vec3f repr
  val yyz : ('n vec_ge3f) repr -> vec3f repr
  val yyw : ('n vec_ge4f) repr -> vec3f repr
  val yzx : ('n vec_ge3f) repr -> vec3f repr
  val yzy : ('n vec_ge3f) repr -> vec3f repr
  val yzz : ('n vec_ge3f) repr -> vec3f repr
  val yzw : ('n vec_ge4f) repr -> vec3f repr
  val ywx : ('n vec_ge4f) repr -> vec3f repr
  val ywy : ('n vec_ge4f) repr -> vec3f repr
  val ywz : ('n vec_ge4f) repr -> vec3f repr
  val yww : ('n vec_ge4f) repr -> vec3f repr

  val zxx : ('n vec_ge3f) repr -> vec3f repr
  val zxy : ('n vec_ge3f) repr -> vec3f repr
  val zxz : ('n vec_ge3f) repr -> vec3f repr
  val zxw : ('n vec_ge4f) repr -> vec3f repr
  val zyx : ('n vec_ge3f) repr -> vec3f repr
  val zyy : ('n vec_ge3f) repr -> vec3f repr
  val zyz : ('n vec_ge3f) repr -> vec3f repr
  val zyw : ('n vec_ge4f) repr -> vec3f repr
  val zzx : ('n vec_ge3f) repr -> vec3f repr
  val zzy : ('n vec_ge3f) repr -> vec3f repr
  val zzz : ('n vec_ge3f) repr -> vec3f repr
  val zzw : ('n vec_ge4f) repr -> vec3f repr
  val zwx : ('n vec_ge4f) repr -> vec3f repr
  val zwy : ('n vec_ge4f) repr -> vec3f repr
  val zwz : ('n vec_ge4f) repr -> vec3f repr
  val zww : ('n vec_ge4f) repr -> vec3f repr

  val wxx : ('n vec_ge4f) repr -> vec3f repr
  val wxy : ('n vec_ge4f) repr -> vec3f repr
  val wxz : ('n vec_ge4f) repr -> vec3f repr
  val wxw : ('n vec_ge4f) repr -> vec3f repr
  val wyx : ('n vec_ge4f) repr -> vec3f repr
  val wyy : ('n vec_ge4f) repr -> vec3f repr
  val wyz : ('n vec_ge4f) repr -> vec3f repr
  val wyw : ('n vec_ge4f) repr -> vec3f repr
  val wzx : ('n vec_ge4f) repr -> vec3f repr
  val wzy : ('n vec_ge4f) repr -> vec3f repr
  val wzz : ('n vec_ge4f) repr -> vec3f repr
  val wzw : ('n vec_ge4f) repr -> vec3f repr
  val wwx : ('n vec_ge4f) repr -> vec3f repr
  val wwy : ('n vec_ge4f) repr -> vec3f repr
  val wwz : ('n vec_ge4f) repr -> vec3f repr
  val www : ('n vec_ge4f) repr -> vec3f repr

  (* TODO: More swizzle operators *)

end


module type Notation = sig

  type 'a repr

  (* NOTE: These operators are a bit janky... really wish we had modular
           implicits in OCaml. :'(

    The idea is that the vertical pipes are used when the argument is a vector.
    This might end up getting weird as we add matrix operations as well!
  *)

  val (!!) : float -> vec1f repr

  val (+) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|+) : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val (-) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|-) : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val ( * ) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|*) : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val (/) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|/) : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

  val (%) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val (|%) : ('n vecf) repr -> vec1f repr -> ('n vecf) repr

end


module Notation (S: S) : Notation

  with type 'a repr = 'a S.repr

= struct

  type 'a repr = 'a S.repr

  let (!!) = S.float

  let (+) = S.add
  let (|+) = S.add_scalar

  let (-) = S.sub
  let (|-) = S.sub_scalar

  let ( * ) = S.mul
  let (|*) = S.mul_scalar

  let (/) = S.div
  let (|/) = S.div_scalar

  let (%) = S.mod_
  let (|%) = S.mod_scalar

end
