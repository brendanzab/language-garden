open Shader_graphics.Shader_types

module Monad = Shader_graphics.Control.Monad

module Cpu = Shader_graphics.Cpu
module Glsl = Shader_graphics.Glsl
module Sdf = Shader_graphics.Sdf
module Shader = Shader_graphics.Shader

module Make (S : Shader.S) = struct

  open Sdf.Make (S)

  (** Shader module extended with utility functions *)
  module S = struct
    include S
    include Shader.Util (S)
  end

  (** An environment with access to a 2D coordinate. *)
  module Env = Monad.Reader.Function (struct type t = vec2f repr end)

  (* Bring notations into scope *)
  open Shader.Notation (S)
  open Env.O


(* $MDX part-begin=scene *)
(** A scene to render, assuming UV coordinates in (-0.5, 0.5) *)
let scene : (vec3f repr) Env.t  =
  (* Some shapes defined using signed distance functions *)
  let* s1 = circle !!0.3 |> move (S.vec2 !!0.0 !!0.0) in
  let* s2 = square !!0.2 |> move (S.vec2 !!0.2 !!0.0) in

  (* Combine the two shapes, meeting at a rounded edge *)
  let shape = union_round s1 s2 !!0.05 in

  (* Colours to use in the background and foreground *)
  let background_color = S.vec3 !!0.35 !!0.45 !!0.50 in
  let shape_color = S.vec3 !!1.0 !!1.0 !!1.0 in

  (* The final output colour to render at the current UV coordinate. *)
  Env.pure (background_color |> overlay ~shape:shape ~color:shape_color)
(* $MDX part-end *)


  (** The scene, rendered as a function from pixel positions to colours. *)
  let image ~dimensions ~position : vec3f repr =
    scene (S.normalise_coords ~dimensions ~position)

end
