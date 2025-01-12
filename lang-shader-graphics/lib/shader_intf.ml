open Data

module type Core = sig

  (** Abstract representation of shader expressions *)
  type 'a repr


  (** {1 Literals} *)

  val float : float -> float repr

  val vec2 : float repr -> float repr -> vec2f repr
  val vec3 : float repr -> float repr -> float repr -> vec3f repr
  val vec4 : float repr -> float repr -> float repr -> float repr -> vec4f repr

  val mat2 : vec2f repr -> vec2f repr -> mat2f repr
  val mat3 : vec3f repr -> vec3f repr -> vec3f repr -> mat3f repr
  val mat4 : vec4f repr -> vec4f repr -> vec4f repr -> vec4f repr -> mat4f repr


  (** {1 Arithemetic operators} *)

  val neg : float repr -> float repr
  val neg_vec : 'n. ('n vecf) repr -> ('n vecf) repr

  val add : float repr -> float repr -> float repr
  val add_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val add_scalar : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

  val sub : float repr -> float repr -> float repr
  val sub_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val sub_scalar : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

  val mul : float repr -> float repr -> float repr
  val mul_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mul_scalar : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

  val div : float repr -> float repr -> float repr
  val div_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val div_scalar : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

  val mod_ : float repr -> float repr -> float repr
  val mod_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mod_scalar : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr


  (** {1 Other functions on scalars and vectors} *)

  val abs : float repr -> float repr
  val abs_vec : 'n. ('n vecf) repr -> ('n vecf) repr

  val clamp : float repr -> min:float repr -> max:float repr -> float repr
  val clamp_vec : 'n. ('n vecf) repr -> min:('n vecf) repr -> max:('n vecf) repr -> ('n vecf) repr
  val clamp_scalar : 'n. ('n vecf) repr -> min:float repr -> max:float repr -> ('n vecf) repr

  val cos : float repr -> float repr
  val cos_vec : 'n. ('n vecf) repr -> ('n vecf) repr

  val dot : 'n. ('n vecf) repr -> ('n vecf) repr -> float repr

  val length : 'n. ('n vecf) repr -> float repr

  val lerp : float repr -> float repr -> float repr -> float repr
  val lerp_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val lerp_scalar : 'n. ('n vecf) repr -> ('n vecf) repr -> float repr -> ('n vecf) repr

  val max : float repr -> float repr -> float repr
  val max_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val min : float repr -> float repr -> float repr
  val min_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val pow : float repr -> float repr -> float repr
  val pow_vec : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val round : float repr -> float repr
  val round_vec : 'n. ('n vecf) repr -> ('n vecf) repr

  val sin : float repr -> float repr
  val sin_vec : 'n. ('n vecf) repr -> ('n vecf) repr

  val smooth_step : lower:float repr -> upper:float repr -> float repr -> float repr
  val smooth_step_vec : 'n. lower:('n vecf) repr -> upper:('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val smooth_step_scalar : 'n. lower:float repr -> upper:float repr -> ('n vecf) repr -> ('n vecf) repr

  val sqrt : float repr -> float repr
  val sqrt_vec : 'n. ('n vecf) repr -> ('n vecf) repr

  val step : edge:float repr -> float repr -> float repr
  val step_vec : 'n. edge:('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val step_scalar : 'n. edge:float repr -> ('n vecf) repr -> ('n vecf) repr

  val tan : float repr -> float repr
  val tan_vec : 'n. ('n vecf) repr -> ('n vecf) repr


  (** {1 Vector combinators} *)

  val map_vec : 'n. (float repr -> float repr) -> ('n vecf) repr -> ('n vecf) repr

  val fold_left_vec : 'a 'n. ('a repr -> float repr -> 'a repr) -> 'a repr -> ('n vecf) repr -> 'a repr


  (** {1 Vector component operations} *)

  (** {2 Component projections} *)

  val get : 'n. 'n Vec.component -> ('n vecf) repr -> float repr

  (** {2 Swizzle operators} *)

  val get2 : 'n. 'n Vec.component * 'n Vec.component -> ('n vecf) repr -> vec2f repr
  val get3 : 'n. 'n Vec.component * 'n Vec.component * 'n Vec.component -> ('n vecf) repr -> vec3f repr
  val get4 : 'n. 'n Vec.component * 'n Vec.component * 'n Vec.component * 'n Vec.component -> ('n vecf) repr -> vec4f repr

  (** {2 Component updates} *)

  val set : 'n. 'n Vec.component ->  float repr -> ('n vecf) repr -> ('n vecf) repr

end

(** A shader language that supports a variety of operators and functions related
    to linear algebra *)
module type S = sig

  include Core

  val zero2 : vec2f repr
  val zero3 : vec3f repr
  val zero4 : vec4f repr

  val max_component2 : vec2f repr -> float repr
  val max_component3 : vec3f repr -> float repr
  val max_component4 : vec4f repr -> float repr

  val min_component2 : vec2f repr -> float repr
  val min_component3 : vec3f repr -> float repr
  val min_component4 : vec4f repr -> float repr

  (** Clamp a scalar to the range [[0.0, 1.0]] *)
  val saturate : float repr -> float repr

  (** Clamp a vector to the range [[0.0, 1.0]] *)
  val saturate_vec : 'n. ('n vecf) repr -> ('n vecf) repr


  (** {1 Coordinate systems} **)

  (* TODO: Support typed coordinate systems? *)

  (** Given some screen dimensions and a position within that screen, normalise
      to UV coordinates in the range [[-0.5, 0.5]], ensuring that the proportions
      remain square by scaling the x axis based on the aspect ratio. *)
  val normalise_coords : dimensions:vec2f repr -> position:vec2f repr -> vec2f repr

  (** Remap UV coordinates from normalised corner coordintes [[0.0, 1.0]] to
      centered cordinates [[-0.5, 0.5]] *)
  val center_coords : 'n. ('n vecf) repr -> ('n vecf) repr

  (** Remap UV coordinates from centered coordinates to [[-0.5, 0.5]] to corner
      coordinates [[0.0, 1.0]] *)
  val corner_coords : 'n. ('n vecf) repr -> ('n vecf) repr


  (** Convenience operators *)
  module O : sig

    (* NOTE: These operators are a bit janky... really wish we had modular
            implicits in OCaml. :'(

      The idea is that the vertical pipes are used when the argument is a vector.
      This might end up getting weird as we add matrix operations as well!
    *)

    val ( !! ) : float -> float repr

    val ( + ) : float repr -> float repr -> float repr
    val ( |+| ) : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
    val ( |+ ) : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

    val ( - ) : float repr -> float repr -> float repr
    val ( |-| ) : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
    val ( |- ) : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

    val (  *  ) : float repr -> float repr -> float repr
    val ( |*| ) : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
    val ( |* ) : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

    val ( / ) : float repr -> float repr -> float repr
    val ( |/| ) : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
    val ( |/ ) : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

    val ( % ) : float repr -> float repr -> float repr
    val ( |%| ) : 'n. ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
    val ( |% ) : 'n. ('n vecf) repr -> float repr -> ('n vecf) repr

    val ( .%{ } ) : 'n. ('n vecf) repr -> 'n Vec.component -> float repr
    val ( .%{ }<- ) : 'n. ('n vecf) repr -> 'n Vec.component ->  float repr -> ('n vecf) repr

  end

  (** Named vector component accessors *)
  module Component : sig

    val x : 'n. 'n vec_ge1f repr -> float repr
    val y : 'n. 'n vec_ge2f repr -> float repr
    val z : 'n. 'n vec_ge3f repr -> float repr
    val w : 'n. 'n vec_ge4f repr -> float repr

    val set_x : 'n. float repr -> 'n vec_ge1f repr -> 'n vec_ge1f repr
    val set_y : 'n. float repr -> 'n vec_ge2f repr -> 'n vec_ge2f repr
    val set_z : 'n. float repr -> 'n vec_ge3f repr -> 'n vec_ge3f repr
    val set_w : 'n. float repr -> 'n vec_ge4f repr -> 'n vec_ge4f repr

    val xx : 'n. 'n vec_ge1f repr -> vec2f repr
    val xy : 'n. 'n vec_ge2f repr -> vec2f repr
    val xz : 'n. 'n vec_ge3f repr -> vec2f repr
    val xw : 'n. 'n vec_ge4f repr -> vec2f repr
    val yx : 'n. 'n vec_ge2f repr -> vec2f repr
    val yy : 'n. 'n vec_ge2f repr -> vec2f repr
    val yz : 'n. 'n vec_ge3f repr -> vec2f repr
    val yw : 'n. 'n vec_ge4f repr -> vec2f repr
    val zx : 'n. 'n vec_ge3f repr -> vec2f repr
    val zy : 'n. 'n vec_ge3f repr -> vec2f repr
    val zz : 'n. 'n vec_ge3f repr -> vec2f repr
    val zw : 'n. 'n vec_ge4f repr -> vec2f repr
    val wx : 'n. 'n vec_ge4f repr -> vec2f repr
    val wy : 'n. 'n vec_ge4f repr -> vec2f repr
    val wz : 'n. 'n vec_ge4f repr -> vec2f repr
    val ww : 'n. 'n vec_ge4f repr -> vec2f repr
    val xxx : 'n. 'n vec_ge1f repr -> vec3f repr
    val xxy : 'n. 'n vec_ge2f repr -> vec3f repr
    val xxz : 'n. 'n vec_ge3f repr -> vec3f repr
    val xxw : 'n. 'n vec_ge4f repr -> vec3f repr
    val xyx : 'n. 'n vec_ge2f repr -> vec3f repr
    val xyy : 'n. 'n vec_ge2f repr -> vec3f repr
    val xyz : 'n. 'n vec_ge3f repr -> vec3f repr
    val xyw : 'n. 'n vec_ge4f repr -> vec3f repr
    val xzx : 'n. 'n vec_ge3f repr -> vec3f repr
    val xzy : 'n. 'n vec_ge3f repr -> vec3f repr
    val xzz : 'n. 'n vec_ge3f repr -> vec3f repr
    val xzw : 'n. 'n vec_ge4f repr -> vec3f repr
    val xwx : 'n. 'n vec_ge4f repr -> vec3f repr
    val xwy : 'n. 'n vec_ge4f repr -> vec3f repr
    val xwz : 'n. 'n vec_ge4f repr -> vec3f repr
    val xww : 'n. 'n vec_ge4f repr -> vec3f repr
    val yxx : 'n. 'n vec_ge2f repr -> vec3f repr
    val yxy : 'n. 'n vec_ge2f repr -> vec3f repr
    val yxz : 'n. 'n vec_ge3f repr -> vec3f repr
    val yxw : 'n. 'n vec_ge4f repr -> vec3f repr
    val yyx : 'n. 'n vec_ge2f repr -> vec3f repr
    val yyy : 'n. 'n vec_ge2f repr -> vec3f repr
    val yyz : 'n. 'n vec_ge3f repr -> vec3f repr
    val yyw : 'n. 'n vec_ge4f repr -> vec3f repr
    val yzx : 'n. 'n vec_ge3f repr -> vec3f repr
    val yzy : 'n. 'n vec_ge3f repr -> vec3f repr
    val yzz : 'n. 'n vec_ge3f repr -> vec3f repr
    val yzw : 'n. 'n vec_ge4f repr -> vec3f repr
    val ywx : 'n. 'n vec_ge4f repr -> vec3f repr
    val ywy : 'n. 'n vec_ge4f repr -> vec3f repr
    val ywz : 'n. 'n vec_ge4f repr -> vec3f repr
    val yww : 'n. 'n vec_ge4f repr -> vec3f repr
    val zxx : 'n. 'n vec_ge3f repr -> vec3f repr
    val zxy : 'n. 'n vec_ge3f repr -> vec3f repr
    val zxz : 'n. 'n vec_ge3f repr -> vec3f repr
    val zxw : 'n. 'n vec_ge4f repr -> vec3f repr
    val zyx : 'n. 'n vec_ge3f repr -> vec3f repr
    val zyy : 'n. 'n vec_ge3f repr -> vec3f repr
    val zyz : 'n. 'n vec_ge3f repr -> vec3f repr
    val zyw : 'n. 'n vec_ge4f repr -> vec3f repr
    val zzx : 'n. 'n vec_ge3f repr -> vec3f repr
    val zzy : 'n. 'n vec_ge3f repr -> vec3f repr
    val zzz : 'n. 'n vec_ge3f repr -> vec3f repr
    val zzw : 'n. 'n vec_ge4f repr -> vec3f repr
    val zwx : 'n. 'n vec_ge4f repr -> vec3f repr
    val zwy : 'n. 'n vec_ge4f repr -> vec3f repr
    val zwz : 'n. 'n vec_ge4f repr -> vec3f repr
    val zww : 'n. 'n vec_ge4f repr -> vec3f repr
    val wxx : 'n. 'n vec_ge4f repr -> vec3f repr
    val wxy : 'n. 'n vec_ge4f repr -> vec3f repr
    val wxz : 'n. 'n vec_ge4f repr -> vec3f repr
    val wxw : 'n. 'n vec_ge4f repr -> vec3f repr
    val wyx : 'n. 'n vec_ge4f repr -> vec3f repr
    val wyy : 'n. 'n vec_ge4f repr -> vec3f repr
    val wyz : 'n. 'n vec_ge4f repr -> vec3f repr
    val wyw : 'n. 'n vec_ge4f repr -> vec3f repr
    val wzx : 'n. 'n vec_ge4f repr -> vec3f repr
    val wzy : 'n. 'n vec_ge4f repr -> vec3f repr
    val wzz : 'n. 'n vec_ge4f repr -> vec3f repr
    val wzw : 'n. 'n vec_ge4f repr -> vec3f repr
    val wwx : 'n. 'n vec_ge4f repr -> vec3f repr
    val wwy : 'n. 'n vec_ge4f repr -> vec3f repr
    val wwz : 'n. 'n vec_ge4f repr -> vec3f repr
    val www : 'n. 'n vec_ge4f repr -> vec3f repr

  end

end
