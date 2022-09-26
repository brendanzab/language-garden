(** {0 Expressions in the GLSL shader language} *)

open ShaderTypes


(** GLSL type *)
type _ ty =
  | Float : vec1f ty
  | Vec2 : vec2f ty
  | Vec3 : vec3f ty
  | Vec4 : vec4f ty
  | Mat2 : mat2f ty
  | Mat3 : mat3f ty
  | Mat4 : mat4f ty

(** Compare two GLSL types for equality *)
val equal_ty : 'a ty -> 'b ty -> bool

(** Compile a GLSL type to a string *)
val string_of_ty : 'a ty -> string


(** A typed GLSL expression *)
type 'a expr

(** Unsafely construct a GLSL expression from a string. *)
val unsafe_expr : string -> 'a ty -> 'a expr

(** Compare two GLSL expressions for equality *)
val equal_expr : 'a expr -> 'b expr -> bool

(** Compile a GLSL expression to a string *)
val string_of_expr : 'a expr -> string

(** Return the type of a GLSL expression *)
val ty_of_expr : 'a expr -> 'a ty

(** A GLSL expression of any type *)
type any_expr =
  | AnyExpr : 'a expr -> any_expr


(** A collection of shared GLSL expressions. This is useful for compiling GLSL,
    where it would be preferrable to share the result of intermediate
    computations. *)
module Locals : sig

  (** The type of the local envitonment *)
  type t

  (** An empty local environment *)
  val empty : t

  (** Iterate through the definitions, in the order in which they should be
      defined in a GLSL shader. *)
  val iter : (string * any_expr -> unit) -> t -> unit

  (** Add a shared definition, avoiding the introduction of common
      sub-expressions. *)
  val define : 'a expr -> t -> 'a expr * t

end


(* An environment that allows for the definition of shared GLSL expressions *)
module Env : sig

  include Control.Monad.State with type t = Locals.t

  (** Define a new local definition *)
  val define_local : 'a expr -> ('a expr) m

end


include Shader.S with type 'a repr = ('a expr) Env.m


(** Utilities for compiling Shadertoy-compatible shaders *)
module Shadertoy : sig

  (** Per-frame static information for Shadertoy-compatible shaders *)
  type uniforms = {
    resolution : vec3f repr;        (** viewport resolution (in pixels) *)
    time : vec1f repr;              (** shader playback time (in seconds) *)
    time_delta : vec1f repr;        (** render time (in seconds) *)
    frame : vec1f repr;             (** shader playback frame *)
    mouse : vec4f repr;             (** mouse pixel coords. xy: current (if MLB down), zw: click *)
    date : vec4f repr;              (** (year, month, day, time in seconds) *)
    sample_rate : vec1f repr;       (** sound sample rate (i.e., 44100) *)
    (* TODO: channel_time : ((vec1f, n4) array) repr *)
    (* TODO: channel_resolution : ((vec3f, n4) array) repr *)
    (* TODO: channel_i : sampler_xx repr *)
  }

  (** An image shader that can be run on https://www.shadertoy.com/. The function
      takes the per-frame uniforms and the pixel (fragment) coordinate as
      arguments and returns color of the pixel. *)
  type image_shader = uniforms -> vec2f repr -> vec3f repr

  (** Compile an image shader to GLSL *)
  val compile_image_shader : image_shader -> string

end
