open ShaderGraphics.ShaderTypes

module Monad = ShaderGraphics.Control.Monad
module Sdf = ShaderGraphics.Sdf
module Shader = ShaderGraphics.Shader


(** A simple scene, implemented using signed distance fields. *)
module MyScene (S : Shader.S) = struct

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
  let gradient_background : (vec3f repr) Env.m =
    let* uv = Env.read in       (* Get the current UV coordinate *)

    (* Some colours to interpolate between *)
    let bottom_color = S.vec3 !!0.35 !!0.45 !!0.50 in
    let top_color = S.vec3 !!0.85 !!0.85 !!0.70 in

    (* Interpolation amount. This is mostly vertical, with a slight tilt to the
       right as a result of mixing in some of the x position. *)
    let amount = uv.%{Y} + (uv.%{X} * !!0.2) in

    Env.pure (S.lerp_scalar bottom_color top_color amount)


  (** A scene to render, assuming UV coordinates in [[-0.5, 0.5]] *)
  let scene : (vec3f repr) Env.m  =
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


let usage out_channel program =
  let program_name = Filename.basename program in
  Printf.fprintf out_channel "USAGE:\n";
  Printf.fprintf out_channel "    %s compile\n" program_name;
  Printf.fprintf out_channel "    %s render\n" program_name;
  Printf.fprintf out_channel "    %s --help | -h\n" program_name

let usage_error program =
  Printf.eprintf "error: unexpected CLI arguments\n";
  usage stderr program;
  exit 1


let () =
  let module Cpu = ShaderGraphics.Cpu in
  let module Glsl = ShaderGraphics.Glsl in

  match Array.to_list Sys.argv with
  (* Compile the scene to a GLSL shader that can be rendered in parallel on the GPU. *)
  | [_; "compile"] ->
      let module Scene = MyScene (Glsl) in
      let open Shader.Notation (Glsl) in

      (* TODO: Render to HTML canvas *)

      Glsl.Shadertoy.compile_image_shader
        (fun uniforms frag_coord ->
          Scene.image
            ~dimensions:(uniforms.resolution |> Glsl.get2 (X, Y))
            ~position:frag_coord)

  (* Render the scene sequentially on the CPU to a PPM image file. *)
  | [_; "render"] ->
      let module Scene = MyScene (Cpu) in
      let open Shader.Notation (Cpu) in

      Cpu.render_ppm ~width:600 ~height:400
        (fun position ->
          Scene.image
            ~dimensions:(vec2 600.0 400.0)
            ~position)

  | [program; "--help" | "-h"] -> usage stdout program
  | program :: _ -> usage_error program
  | [] -> usage_error "main"
