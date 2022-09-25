open MathTypes


(** A simple scene, implemented using signed distance fields. *)
module MyScene (M : Math.S) = struct

  open Sdf.Make (M)

  (** An environment with access to a 2D coordinate. *)
  module Env = Control.Monad.Reader (struct type t = vec2f repr end)

  (* Bring notations into scope *)
  open Math.Notation (M)
  open Control.Monad.Notation (Env)


  (** Gradient background *)
  let background : (vec3f repr) Env.m =
    let* uv = Env.ask in        (* Get the current UV coordinate *)
    let uv = uv |+ !!0.5 in     (* Remap UV coordinates from (-0.5, 0.5) to (0, 1) *)

    (* Some colours to interpolate between *)
    let bottom_color = M.vec3 !!0.35 !!0.45 !!0.50 in
    let top_color = M.vec3 !!0.85 !!0.85 !!0.70 in

    (* Interpolation amount. This is mostly vertical, with a slight tilt to the
       right as a result of mixing in some of the x position. *)
    let amount = M.y uv + (M.x uv * !!0.2) in

    Env.pure (M.lerp_scalar bottom_color top_color amount)


  (** A scene to render, assuming UV coordinates in (-0.5, 0.5) *)
  let scene : (vec3f repr) Env.m  =
    (* Colour to use in the background *)
    let* bg = background in

    (* Some shapes defined using signed distance functions *)
    let* s1 = circle !!0.05 |> repeat ~spacing:(M.vec2 !!0.2 !!0.2) in
    let* s2 = square !!0.15 |> move (M.vec2 !!0.2 !!0.2) |> reflect in

    (* Colour to use in the foreground *)
    let shape_color = M.vec3 !!1.0 !!1.0 !!1.0 in

    (* Composite the distance fields, returning the final output colour for the
       current UV coordinate. *)
    Env.pure (overlay ~bg ~fg:shape_color (union s1 s2))

end


let () =
  (* Construct an implementation of our scene that can be compiled to a GLSL
     shader that can be rendered in parallel on the GPU. *)
  let module Scene = MyScene (Glsl) in
  let open Math.Notation (Glsl) in

  (* TODO: Render to HTML canvas *)

  let glsl = Glsl.Shadertoy.compile_image_shader (fun uniforms frag_coord ->
    let resolution = uniforms.resolution in

    let uv = frag_coord |/| Glsl.xy resolution in   (* Normalise UV coordinates to (0, 1) *)
    let uv = uv |- !!0.5 in                         (* Remap UV coordinates to (-0.5, 0.5) *)

    (* Fix the aspect ratio of the x axis *)
    let aspect = Glsl.x resolution / Glsl.y resolution in
    let uv = Glsl.vec2 (Glsl.x uv * aspect) (Glsl.y uv) in

    Scene.scene uv)
  in

  Format.printf "%s\n" glsl
