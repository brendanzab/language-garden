(** {1 Typed shader language expressions *)

open ShaderTypes


(** A shader language that supports a variety of operators and functions related
    to linear algebra *)
module type S = sig

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

  val pow : float repr -> float repr -> float repr
  val pow_vec : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr

  val round : float repr -> float repr
  val round_vec : ('n vecf) repr -> ('n vecf) repr

  val sin : float repr -> float repr
  val sin_vec : ('n vecf) repr -> ('n vecf) repr

  val smooth_step : lower:float repr -> upper:float repr -> float repr -> float repr
  val smooth_step_vec : lower:('n vecf) repr -> upper:('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val smooth_step_scalar : lower:float repr -> upper:float repr -> ('n vecf) repr -> ('n vecf) repr

  val sqrt : float repr -> float repr
  val sqrt_vec : ('n vecf) repr -> ('n vecf) repr

  val step : edge:float repr -> float repr -> float repr
  val step_vec : edge:('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val step_scalar : edge:float repr -> ('n vecf) repr -> ('n vecf) repr

  val tan : float repr -> float repr
  val tan_vec : ('n vecf) repr -> ('n vecf) repr


  (** {1 Vector combinators} *)

  val map_vec : (float repr -> float repr) -> ('n vecf) repr -> ('n vecf) repr

  val fold_left_vec : ('a repr -> float repr -> 'a repr) -> 'a repr -> ('n vecf) repr -> 'a repr


  (** {1 Vector component operations} *)

  (** {2 Component projections} *)

  val get : 'n component -> ('n vecf) repr -> float repr

  (** {2 Swizzle operators} *)

  val get2 : 'n component * 'n component -> ('n vecf) repr -> vec2f repr
  val get3 : 'n component * 'n component * 'n component -> ('n vecf) repr -> vec3f repr
  val get4 : 'n component * 'n component * 'n component * 'n component -> ('n vecf) repr -> vec4f repr

  (** {2 Component updates} *)

  val set : 'n component ->  float repr -> ('n vecf) repr -> ('n vecf) repr

end


module type Notation = sig

  type 'a repr

  (* NOTE: These operators are a bit janky... really wish we had modular
           implicits in OCaml. :'(

    The idea is that the vertical pipes are used when the argument is a vector.
    This might end up getting weird as we add matrix operations as well!
  *)

  val ( !! ) : float -> float repr

  val ( + ) : float repr -> float repr -> float repr
  val ( |+| ) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val ( |+ ) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val ( - ) : float repr -> float repr -> float repr
  val ( |-| ) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val ( |- ) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val (  *  ) : float repr -> float repr -> float repr
  val ( |*| ) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val ( |* ) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val ( / ) : float repr -> float repr -> float repr
  val ( |/| ) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val ( |/ ) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val ( % ) : float repr -> float repr -> float repr
  val ( |%| ) : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val ( |% ) : ('n vecf) repr -> float repr -> ('n vecf) repr

  val ( .%{ } ) : ('n vecf) repr -> 'n component -> float repr
  val ( .%{ }<- ) : ('n vecf) repr -> 'n component ->  float repr -> ('n vecf) repr

end


module Notation (S : S) : Notation

  with type 'a repr = 'a S.repr

= struct

  type 'a repr = 'a S.repr

  let ( !! ) = S.float

  let ( + ) = S.add
  let ( |+| ) = S.add_vec
  let ( |+ ) = S.add_scalar

  let ( - ) = S.sub
  let ( |-| ) = S.sub_vec
  let ( |- ) = S.sub_scalar

  let (  *  ) = S.mul
  let ( |*| ) = S.mul_vec
  let ( |* ) = S.mul_scalar

  let ( / ) = S.div
  let ( |/| ) = S.div_vec
  let ( |/ ) = S.div_scalar

  let ( % ) = S.mod_
  let ( |%| ) = S.mod_vec
  let ( |% ) = S.mod_scalar

  let ( .%{ } ) v c = S.get c v
  let ( .%{ }<- ) v c s = S.set c s v

end


(** Utilitiy functions derived from a shader module *)
module Util (S : S) : sig

  val zero2 : vec2f S.repr
  val zero3 : vec3f S.repr
  val zero4 : vec4f S.repr

  val max_component2 : vec2f S.repr -> float S.repr
  val max_component3 : vec3f S.repr -> float S.repr
  val max_component4 : vec4f S.repr -> float S.repr

  val min_component2 : vec2f S.repr -> float S.repr
  val min_component3 : vec3f S.repr -> float S.repr
  val min_component4 : vec4f S.repr -> float S.repr

  (** Clamp a scalar to the range [[0.0, 1.0]] *)
  val saturate : float S.repr -> float S.repr

  (** Clamp a vector to the range [[0.0, 1.0]] *)
  val saturate_vec : ('n vecf) S.repr -> ('n vecf) S.repr


  (** {1 Coordinate systems} **)

  (* TODO: Support typed coordinate systems? *)

  (** Given some screen dimensions and a position within that screen, normalise
      to UV coordinates in the range [[-0.5, 0.5]], ensuring that the proportions
      remain square by scaling the x axis based on the aspect ratio. *)
  val normalise_coords : dimensions:vec2f S.repr -> position:vec2f S.repr -> vec2f S.repr

  (** Remap UV coordinates from normalised corner coordintes [[0.0, 1.0]] to
      centered cordinates [[-0.5, 0.5]] *)
  val center_coords : ('n vecf) S.repr -> ('n vecf) S.repr

  (** Remap UV coordinates from centered coordinates to [[-0.5, 0.5]] to corner
      coordinates [[0.0, 1.0]] *)
  val corner_coords : ('n vecf) S.repr -> ('n vecf) S.repr

end = struct

  open Notation (S)


  let saturate s = S.clamp s ~min:!!0.0 ~max:!!1.0
  let saturate_vec v = S.clamp_scalar v ~min:!!0.0 ~max:!!1.0

  let zero2 = S.vec2 !!0.0 !!0.0
  let zero3 = S.vec3 !!0.0 !!0.0 !!0.0
  let zero4 = S.vec4 !!0.0 !!0.0 !!0.0 !!0.0

  let max_component2 v = S.max v.%{X} v.%{Y}
  let max_component3 v = S.max (max_component2 v) v.%{Z}
  let max_component4 v = S.max (max_component2 v) v.%{W}

  let min_component2 v = S.min v.%{X} v.%{Y}
  let min_component3 v = S.min (min_component2 v) v.%{Z}
  let min_component4 v = S.min (min_component2 v) v.%{W}


  let center_coords uv = uv |- !!0.5

  let corner_coords uv = uv |+ !!0.5

  let normalise_coords ~dimensions ~position =
    let uv = center_coords (position |/| dimensions) in
    (* Fix the aspect ratio of the x axis to remove warping *)
    let aspect = dimensions.%{X} / dimensions.%{Y} in
    uv.%{X} <- uv.%{X} * aspect

end


(** Creates a module of named vector component accessors for a shader language *)
module Component (S : S) = struct

  (* TODO: Use a PPX instead?

     Eg. https://github.com/Octachron/tensority/blob/master/ppx/ppx_tensority.ml
  *)

  let x v = S.get X v
  let y v = S.get Y v
  let z v = S.get Z v
  let w v = S.get W v

  let set_x sx v = S.set X sx v
  let set_y sy v = S.set Y sy v
  let set_z sz v = S.set Z sz v
  let set_w sw v = S.set W sw v

  let xx v = S.get2 (X, X) v
  let xy v = S.get2 (X, Y) v
  let xz v = S.get2 (X, Z) v
  let xw v = S.get2 (X, W) v

  let yx v = S.get2 (Y, X) v
  let yy v = S.get2 (Y, Y) v
  let yz v = S.get2 (Y, Z) v
  let yw v = S.get2 (Y, W) v

  let zx v = S.get2 (Z, X) v
  let zy v = S.get2 (Z, Y) v
  let zz v = S.get2 (Z, Z) v
  let zw v = S.get2 (Z, W) v

  let wx v = S.get2 (W, X) v
  let wy v = S.get2 (W, Y) v
  let wz v = S.get2 (W, Z) v
  let ww v = S.get2 (W, W) v

  let xxx v = S.get3 (X, X, X) v
  let xxy v = S.get3 (X, X, Y) v
  let xxz v = S.get3 (X, X, Z) v
  let xxw v = S.get3 (X, X, W) v
  let xyx v = S.get3 (X, Y, X) v
  let xyy v = S.get3 (X, Y, Y) v
  let xyz v = S.get3 (X, Y, Z) v
  let xyw v = S.get3 (X, Y, W) v
  let xzx v = S.get3 (X, Z, X) v
  let xzy v = S.get3 (X, Z, Y) v
  let xzz v = S.get3 (X, Z, Z) v
  let xzw v = S.get3 (X, Z, W) v
  let xwx v = S.get3 (X, W, X) v
  let xwy v = S.get3 (X, W, Y) v
  let xwz v = S.get3 (X, W, Z) v
  let xww v = S.get3 (X, W, W) v

  let yxx v = S.get3 (Y, X, X) v
  let yxy v = S.get3 (Y, X, Y) v
  let yxz v = S.get3 (Y, X, Z) v
  let yxw v = S.get3 (Y, X, W) v
  let yyx v = S.get3 (Y, Y, X) v
  let yyy v = S.get3 (Y, Y, Y) v
  let yyz v = S.get3 (Y, Y, Z) v
  let yyw v = S.get3 (Y, Y, W) v
  let yzx v = S.get3 (Y, Z, X) v
  let yzy v = S.get3 (Y, Z, Y) v
  let yzz v = S.get3 (Y, Z, Z) v
  let yzw v = S.get3 (Y, Z, W) v
  let ywx v = S.get3 (Y, W, X) v
  let ywy v = S.get3 (Y, W, Y) v
  let ywz v = S.get3 (Y, W, Z) v
  let yww v = S.get3 (Y, W, W) v

  let zxx v = S.get3 (Z, X, X) v
  let zxy v = S.get3 (Z, X, Y) v
  let zxz v = S.get3 (Z, X, Z) v
  let zxw v = S.get3 (Z, X, W) v
  let zyx v = S.get3 (Z, Y, X) v
  let zyy v = S.get3 (Z, Y, Y) v
  let zyz v = S.get3 (Z, Y, Z) v
  let zyw v = S.get3 (Z, Y, W) v
  let zzx v = S.get3 (Z, Z, X) v
  let zzy v = S.get3 (Z, Z, Y) v
  let zzz v = S.get3 (Z, Z, Z) v
  let zzw v = S.get3 (Z, Z, W) v
  let zwx v = S.get3 (Z, W, X) v
  let zwy v = S.get3 (Z, W, Y) v
  let zwz v = S.get3 (Z, W, Z) v
  let zww v = S.get3 (Z, W, W) v

  let wxx v = S.get3 (W, X, X) v
  let wxy v = S.get3 (W, X, Y) v
  let wxz v = S.get3 (W, X, Z) v
  let wxw v = S.get3 (W, X, W) v
  let wyx v = S.get3 (W, Y, X) v
  let wyy v = S.get3 (W, Y, Y) v
  let wyz v = S.get3 (W, Y, Z) v
  let wyw v = S.get3 (W, Y, W) v
  let wzx v = S.get3 (W, Z, X) v
  let wzy v = S.get3 (W, Z, Y) v
  let wzz v = S.get3 (W, Z, Z) v
  let wzw v = S.get3 (W, Z, W) v
  let wwx v = S.get3 (W, W, X) v
  let wwy v = S.get3 (W, W, Y) v
  let wwz v = S.get3 (W, W, Z) v
  let www v = S.get3 (W, W, W) v

end
