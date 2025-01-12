open Shader_graphics.Data

module Shader = Shader_graphics.Shader

(** Construct a scene for a shader language *)
module type F = functor (S : Shader.S) -> sig

  (** Build an image for the shader language [S] *)
  val image : dimensions:vec2f S.repr -> position:vec2f S.repr -> vec3f S.repr

end
