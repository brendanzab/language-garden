(** {0 Expressions in the GLSL shader language} *)

open MathTypes


(** GLSL type *)
type _ ty =
  | Float : float ty
  | Vec2 : vec2f ty
  | Vec3 : vec3f ty
  | Vec4 : vec4f ty
  | Mat2 : mat2f ty
  | Mat3 : mat3f ty
  | Mat4 : mat4f ty


(** A typed GLSL expression *)
type 'a expr

(** Unsafely construct a GLSL expression from a string. *)
val unsafe_expr : string -> 'a ty -> 'a expr

(** Return a GLSL expression, as a string *)
val string_of_expr : 'a expr -> string

(** Return the type of a GLSL expression *)
val ty_of_expr : 'a expr -> 'a ty


(** An environment that allows for the definition of shared computations. This
    is useful for compiling GLSL, where it would be preferrable to share the
    result of intermediate computations. *)
module Env : sig

  include Control.Monad.S

  type entry = {
    def : string;
    ty : string;
  }

  type locals

  val empty_locals : locals
  val iter_locals : (string * entry -> unit) -> locals -> unit


  val run : locals -> 'a m -> 'a * locals

end

include Math.S with type 'a repr = ('a expr) Env.m


(** Utilities for compiling Shadertoy-compatible shaders *)
module Shadertoy : sig

  (** Per-frame static information for Shadertoy-compatible shaders *)
  type uniforms = {
    resolution : vec3f repr;        (** viewport resolution (in pixels) *)
    time : float repr;              (** shader playback time (in seconds) *)
    time_delta : float repr;        (** render time (in seconds) *)
    frame : float repr;             (** shader playback frame *)
    mouse : vec4f repr;             (** mouse pixel coords. xy: current (if MLB down), zw: click *)
    date : vec4f repr;              (** (year, month, day, time in seconds) *)
    sample_rate : float repr;       (** sound sample rate (i.e., 44100) *)
    (* TODO: channel_time : ((float, n4) array) repr *)
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
