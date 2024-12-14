(** {0 CPU based shader language} *)

(** This implements a shader language natively in OCaml. This is useful for
    testing the shader language and SDFs without needing to interface with
    graphics APIs. It could also be useful for implementing constant folding
    optimisations in the future. *)

open Storage


include Shader.S with type 'a repr = 'a

(** An image shader to be run on the CPU. The function takes a pixel (fragment)
    coordinate as an argument and returns the color that should be rendered at
    that pixel. *)
type image_shader = vec2f repr -> vec3f repr

(** Render the shader sequentially on the CPU to a PPM image file, using a
    coordinate system that starts from the bottom-left corner of the screen
    for compatibility with OpenGL and Vulkan style shaders. *)
val render_ppm : width:int -> height:int -> image_shader -> unit
