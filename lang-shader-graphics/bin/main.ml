open Shader_graphics.Storage

module Monad = Shader_graphics.Control.Monad

module Cpu = Shader_graphics.Cpu
module Glsl = Shader_graphics.Glsl
module Sdf = Shader_graphics.Sdf
module Shader = Shader_graphics.Shader
module Examples = Shader_graphics_examples


(** {1 Example scenes} *)

module type Scene = sig

  val glsl_image : dimensions:vec2f Glsl.repr -> position:vec2f Glsl.repr -> vec3f Glsl.repr
  val cpu_image : dimensions:vec2f Cpu.repr -> position:vec2f Cpu.repr -> vec3f Cpu.repr

end

module Scene (S : Examples.Scene.F) : Scene = struct

  module Glsl_scene = S (Glsl)
  module Cpu_scene = S (Cpu)

  let glsl_image = Glsl_scene.image
  let cpu_image = Cpu_scene.image

end

module Basic = Scene (Examples.Basic.Make)
module Readme = Scene (Examples.Readme.Make)


let example : string -> (module Scene) =
  function
  | "basic" -> (module Basic)
  | "readme" -> (module Readme)
  | name -> Printf.eprintf "unkown scene '%s'" name; exit 1


(** {1 Subcommands} *)

let compile_cmd scene =
  (* TODO: Render to HTML canvas *)

  Glsl.Shadertoy.compile_image_shader
    (fun uniforms frag_coord ->
      let (module Scene) = example scene in
      Scene.glsl_image
        ~dimensions:(uniforms.resolution |> Glsl.get2 (X, Y))
        ~position:frag_coord)

let render_cmd scene =
  Cpu.render_ppm ~width:600 ~height:400
    (fun position ->
      let (module Scene) = example scene in
      Scene.cpu_image
        ~dimensions:(Vec.[600.0; 400.0])
        ~position)

let list_cmd () =
  print_endline "basic";
  print_endline "readme"


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let scene : string Term.t =
    Arg.(required
      & opt (some & string) None
      & info ["scene"] ~docv:"NAME" ~doc:"The scene to use.")
  in

  Cmd.group (Cmd.info "shader-graphics") [
    Cmd.v (Cmd.info "compile" ~doc:"Compile the scene to a GLSL shader that can be rendered in parallel on the GPU.")
      Term.(const compile_cmd $ scene);
    Cmd.v (Cmd.info "render" ~doc:"Render the scene sequentially on the CPU to a PPM image file.")
      Term.(const render_cmd $ scene);
    Cmd.v (Cmd.info "list" ~doc:"list the available scenes")
      Term.(const list_cmd $ const ());
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
