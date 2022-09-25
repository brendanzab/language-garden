open MathTypes


(** A simple scene, implemented in terms of signed distance fields. *)
module MyScene (M : Math.S) = struct

  open Sdf.Make (M)

  (** An environment with access to a shared 2D coordinate. This gives us access
      to binding operators that make it cleaner to construct functions that
      depend on a UV coordinate. *)
  module Env = Control.Monad.Reader (struct
    type t = vec2f repr
  end)

  (* Bring notations into scope *)
  open Math.Notation (M)
  open Control.Monad.Notation (Env)

  let f = M.float
  let vec2f x y = M.vec2 (f x) (f y)
  let vec3f x y z = M.vec3 (f x) (f y) (f z)

  (** Vertical gradient *)
  let background : (vec3f repr) Env.m =
    let* uv = Env.ask in
    (* Remap UV coordinates from (-0.5, 0.5) to (0, 1) *)
    let uv = uv |+ f 0.5 in

    let bottom_color = vec3f 0.35 0.45 0.50 in
    let top_color = vec3f 0.85 0.85 0.70 in
    let amount = M.y uv + (M.x uv * f 0.2) in

    Env.pure (M.lerp_vs bottom_color top_color amount)

  (** The composed scene *)
  let scene : (vec3f repr) Env.m  =
    let* bg = background in
    let* s1 = circle (f 0.05) |> repeat ~spacing:(vec2f 0.2 0.2) in
    let* s2 = square (f 0.15) |> move (vec2f 0.2 0.2) |> reflect in

    let shapeColor = vec3f 1.0 1.0 1.0 in

    Env.pure (overlay ~bg ~fg:shapeColor (union s1 s2))

end


let () =
  (* Construct an implementation of our scene that can be compiled to a GLSL
     shader that can be rendered in parallel on the GPU. *)
  let module S = MyScene (Glsl) in

  (* TODO: Move top-level setup into Glsl module *)
  (* TODO: Render to HTML canvas *)

  let uv = Glsl.unsafe_expr "uv" Vec2 in
  let (color, locals) = Glsl.Env.run Glsl.Env.empty_locals (S.scene (Glsl.Env.pure uv)) in

  Format.printf "// The main entrypoint of the shader.\n";
  Format.printf "//\n";
  Format.printf "// Copy and paste this into %s to see the output.\n" "https://www.shadertoy.com/new";
  Format.printf "void mainImage(out vec4 fragColor, in vec2 fragCoord) {\n";
  Format.printf "  // Normalise the UV coordinates to (-0.5, 0.5)\n";
  Format.printf "  vec2 uv = fragCoord / iResolution.xy - 0.5;\n";
  Format.printf "  // Fix the aspect ratio of the x axis\n";
  Format.printf "  uv.x *= iResolution.x / iResolution.y;\n";
  Format.printf "\n";
  Format.printf "  // Local bindings\n";
  locals |> Glsl.Env.iter_locals (fun (name, Glsl.Env.{ def; ty }) ->
    Format.printf "  %s %s = %s;\n" ty name def);
  Format.printf "\n";
  Format.printf "  // Compute the colour for this UV coordinate.\n";
  Format.printf "  vec3 color = %s;\n" (Glsl.string_of_expr color);
  Format.printf "\n";
  Format.printf "  // Output to screen\n";
  Format.printf "  fragColor = vec4(color,1.0);\n";
  Format.printf "}\n";

  ()
