open ShaderGraphics.ShaderTypes

module Monad = ShaderGraphics.Control.Monad

module Cpu = ShaderGraphics.Cpu
module Glsl = ShaderGraphics.Glsl
module Sdf = ShaderGraphics.Sdf
module Shader = ShaderGraphics.Shader

module Make (S : Shader.S) = struct

  open Sdf.Make (S)

  (** Shader module extended with utility functions *)
  module S = struct
    include S
    include Shader.Util (S)
  end

  (** An environment with access to a 2D coordinate. *)
  module Env = Monad.FunctionReader (struct type t = vec2f repr end)

  (* Bring notations into scope *)
  open Shader.Notation (S)
  open Monad.Notation (Env)


  (** Gradient background, assuming UV coordinates in [[0.0, 1.0]] *)
  let gradient_background : (vec3f repr) Env.t =
    let* uv = Env.read in       (* Get the current UV coordinate *)

    (* Some colours to interpolate between *)
    let bottom_color = S.vec3 !!0.35 !!0.45 !!0.50 in
    let top_color = S.vec3 !!0.85 !!0.85 !!0.70 in

    (* Interpolation amount. This is mostly vertical, with a slight tilt to the
       right as a result of mixing in some of the x position. *)
    let amount = uv.%{Y} + (uv.%{X} * !!0.2) in

    Env.pure (S.lerp_scalar bottom_color top_color amount)


  (** A scene to render, assuming UV coordinates in [[-0.5, 0.5]] *)
  let scene : (vec3f repr) Env.t  =
    (* Colour to use in the background *)
    let* background = Env.scope S.corner_coords gradient_background in

    (* Some shapes defined using signed distance functions *)
    let* s1 = circle !!0.3 |> move (S.vec2 !!0.0 !!0.0) in
    let* s2 = square !!0.2 |> move (S.vec2 !!0.2 !!0.0) in

    (* Combine the two shapes, meeting at a rounded edge *)
    let shape1 = union_round s1 s2 !!0.05 in
    let shape1_color = S.vec3 !!1.0 !!1.0 !!1.0 in

    (* A box to draw over the above shape *)
    let* box = rectangle (S.vec2 !!0.3 !!0.2) |> move (S.vec2 !!(-0.3) !!(-0.2)) in
    let* line1 = segment (S.vec2 !!(-0.1) !!0.1) (S.vec2 !!0.25 !!0.25) ~radius:!!0.005 in
    let* line2 = segment_y !!0.4 ~radius:!!0.005 |> move (S.vec2 !!0.2 !!(-0.3)) in

    let shape2 = union box (union line1 line2) in
    let shape2_color = S.vec3 !!0.25 !!0.25 !!0.25 in

    (* The final output colour to render at the current UV coordinate. *)
    Env.pure (background
      |> overlay ~shape:shape1 ~color:shape1_color
      |> overlay ~shape:shape2 ~color:shape2_color)


  (** The scene, rendered as a function from pixel positions to colours. *)
  let image ~dimensions ~position : vec3f repr =
    scene (S.normalise_coords ~dimensions ~position)

end
