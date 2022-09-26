open ShaderTypes


(** A simple scene, implemented using signed distance fields. *)
module MyScene (S : Shader.S) = struct

  open Sdf.Make (S)

  (** An environment with access to a 2D coordinate. *)
  module Env = Control.Monad.FunctionReader (struct type t = vec2f repr end)

  (* Bring notations into scope *)
  open Shader.Notation (S)
  open Control.Monad.Notation (Env)


  (** Gradient background *)
  let background : (vec3f repr) Env.m =
    let* uv = Env.ask in        (* Get the current UV coordinate *)
    let uv = uv |+ !!0.5 in     (* Remap UV coordinates from [-0.5, 0.5] to [0, 1] *)

    (* Some colours to interpolate between *)
    let bottom_color = S.vec3 !!0.35 !!0.45 !!0.50 in
    let top_color = S.vec3 !!0.85 !!0.85 !!0.70 in

    (* Interpolation amount. This is mostly vertical, with a slight tilt to the
       right as a result of mixing in some of the x position. *)
    let amount = S.y uv + (S.x uv * !!0.2) in

    Env.pure (S.lerp_scalar bottom_color top_color amount)


  (** A scene to render, assuming UV coordinates in \[-0.5, 0.5\] *)
  let scene : (vec3f repr) Env.m  =
    (* Colour to use in the background *)
    let* bg = background in

    (* Some shapes defined using signed distance functions *)
    let* s1 = circle !!0.3 |> move (S.vec2 !!0.0 !!0.0) in
    let* s2 = square !!0.2 |> move (S.vec2 !!0.2 !!0.0) in

    (* Combine the two shapes, meeting at a rounded edge *)
    let shape = union_round s1 s2 !!0.05 in
    let shape_color = S.vec3 !!1.0 !!1.0 !!1.0 in

    (* A box to draw over the above shape *)
    let* box = rectangle (S.vec2 !!0.3 !!0.2) |> move (S.vec2 !!(-0.3) !!(-0.2)) in
    let box_color = S.vec3 !!0.25 !!0.25 !!0.25 in

    (* The final output colour to render at the current UV coordinate. *)
    Env.pure (overlay ~bg:(overlay ~bg ~fg:shape_color shape) ~fg:box_color box)


  (** The scene, rendered as a function from pixel positions to colours. *)
  let image resolution frag_coord =
    let uv = frag_coord |/| S.xy resolution in   (* Normalise UV coordinates to [0, 1] *)
    let uv = uv |- !!0.5 in                      (* Remap UV coordinates to [-0.5, 0.5] *)

    (* Fix the aspect ratio of the x axis to remove warping *)
    let aspect = S.x resolution / S.y resolution in
    let uv = S.vec2 (S.x uv * aspect) (S.y uv) in

    scene uv

end


let () =
  match Array.to_list Sys.argv with
  (* Compile our scene to a GLSL shader program that can be rendered in parallel
     on the GPU. *)
  | [_; "--glsl"] | [_] ->
      let module Scene = MyScene (Glsl) in
      let open Shader.Notation (Glsl) in

      (* TODO: Render to HTML canvas *)

      Glsl.Shadertoy.compile_image_shader
        (fun uniforms frag_coord ->
          Scene.image uniforms.resolution frag_coord)

  (* Render our scene (slowly!) on the CPU to a PPM image file. *)
  | [_; "--ppm"] ->
      let module Scene = MyScene (Cpu) in
      let open Shader.Notation (Cpu) in

      Cpu.render_ppm ~width:600 ~height:400
        (Scene.image (Cpu.vec2 600.0 400.0))

  | _ ->
    Format.eprintf "invalid CLI arguments";
    exit 1
