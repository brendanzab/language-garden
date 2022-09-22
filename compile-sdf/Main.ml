module type Sdf = sig
  type scalar
  type vec2
  type vec3
  type sdf2

  val (let*) : sdf2 -> (scalar -> sdf2) -> sdf2
  val pure : scalar -> sdf2

  val float : float -> scalar
  val vec2 : scalar -> scalar -> vec2
  val vec3 : scalar -> scalar -> scalar -> vec3

  val circle : radius:scalar -> pos:vec2 -> sdf2
  val square : radius:scalar -> pos:vec2 -> sdf2

  val union : scalar -> scalar -> scalar
  val intersect : scalar -> scalar -> scalar
  val subtract : scalar -> scalar -> scalar

  val mirror_x : sdf2 -> sdf2
  val mix : bg:vec3 -> fg:vec3 -> shape:scalar -> scalar
end

module Glsl : Sdf
  with type sdf2 = string -> string
= struct
  type scalar = string
  type vec2 = string
  type vec3 = string
  type sdf2 = string -> string

  let (let*) sdf f uv =
    f (sdf uv) uv

  let pure s _ = s

  let float = string_of_float
  let vec2 = Format.sprintf "vec2(%s, %s)"
  let vec3 = Format.sprintf "vec3(%s, %s, %s)"

  let circle ~radius ~pos uv =
    Format.sprintf "(length(%s - %s) - %s)" uv pos radius

  let square ~radius ~pos uv =
    let x = Format.sprintf "%s.x - %s.x" uv pos in
    let y = Format.sprintf "%s.y - %s.y" uv pos in
    Format.sprintf "(max(abs(%s), abs(%s)) - %s)" x y radius

  let union d1 d2 = Format.sprintf "min(%s, %s)" d1 d2
  let intersect d1 d2 = Format.sprintf "max(%s, %s)" d1 d2
  let subtract d1 d2 = Format.sprintf "max(%s, -%s)" d1 d2

  let mirror_x sdf uv =
    (* FIXME: Improve sharing of SDF computation *)
    sdf (Format.sprintf "vec2(abs(%s.x), %s.y)" uv uv)

  let mix ~bg ~fg ~shape =
    Format.sprintf "mix(%s, %s, step(0.0, %s))" bg fg shape

end


module MyScene (S : Sdf) = struct
  open S

  let f = float

  let scene =
    let* s1 = mirror_x (circle ~radius:(f 0.3) ~pos:(vec2 (f 0.0) (f 0.0))) in
    let* s2 = square ~radius:(f 0.2) ~pos:(vec2 (f 0.2) (f 0.0)) in
    let shapeColor = vec3 (f 1.0) (f 1.0) (f 1.0) in

    pure (mix ~shape:(union s1 s2)
      ~bg:(vec3 (f 0.0) (f 0.0) (f 0.0))
      ~fg:shapeColor)
end


let () =
let module S = MyScene (Glsl) in

  (* Copy and paste this into https://www.shadertoy.com/new *)

  Format.printf "void mainImage(out vec4 fragColor, in vec2 fragCoord) {\n";
  Format.printf "  // Normalise the UV coordinates to <-0.5,0.5>\n";
  Format.printf "  vec2 uv = fragCoord / iResolution.xy - 0.5;\n";
  Format.printf "  // Fix the aspect ratio of the x axis\n";
  Format.printf "  uv.x *= iResolution.x / iResolution.y;\n";
  Format.printf "\n";
  Format.printf "  // Compute the colour for this UV coordinate.\n";
  Format.printf "  vec3 col = %s;" (S.scene "uv");
  Format.printf "\n";
  Format.printf "  // Output to screen\n";
  Format.printf "  fragColor = vec4(col,1.0);\n";
  Format.printf "}\n";

  ()
