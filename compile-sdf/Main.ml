type ('a, 'rest) vec1n = ('a * 'rest)
type ('a, 'rest) vec2n = ('a * ('a, 'rest) vec1n)
type ('a, 'rest) vec3n = ('a * ('a, 'rest) vec2n)
type ('a, 'rest) vec4n = ('a * ('a, 'rest) vec3n)

type vec2 = (float, unit) vec2n
type vec3 = (float, unit) vec3n
type vec4 = (float, unit) vec4n


module type Sdf = sig
  type 'a repr

  (** A signed distance function from a 2D point to a distance to the boundary
      of a surface. Points outside the surface return positive values, and
      points inside the surface return negative values. *)
  type sdf2 = vec2 repr -> float repr

  val (let*) : sdf2 -> (float repr -> sdf2) -> sdf2
  val pure : float repr -> sdf2

  val float : float -> float repr
  val vec2 : float repr -> float repr -> vec2 repr
  val vec3 : float repr -> float repr -> float repr -> vec3 repr
  val vec4 : float repr -> float repr -> float repr -> float repr -> vec4 repr

  val get_x : (('a, 'rest) vec1n) repr -> 'a repr
  val get_y : (('a, 'rest) vec2n) repr -> 'a repr
  val get_z : (('a, 'rest) vec3n) repr -> 'a repr
  val get_w : (('a, 'rest) vec4n) repr -> 'a repr

  val circle : float repr -> sdf2
  val square : float repr -> sdf2

  val union : float repr -> float repr -> float repr
  val intersect : float repr -> float repr -> float repr
  val subtract : float repr -> float repr -> float repr

  val move : vec2 repr -> sdf2 -> sdf2
  val mirror_x : sdf2 -> sdf2

  val mix : bg:(vec3 repr) -> fg:(vec3 repr) -> shape:(float repr) -> float repr
end

module Glsl : Sdf
  with type 'a repr = string
= struct
  type 'a repr = string

  type sdf2 = vec2 repr -> float repr

  let (let*) sdf f uv =
    f (sdf uv) uv

  let pure s _ = s


  let float = string_of_float
  let vec2 = Format.sprintf "vec2(%s, %s)"
  let vec3 = Format.sprintf "vec3(%s, %s, %s)"
  let vec4 = Format.sprintf "vec4(%s, %s, %s, %s)"


  let get_x = Format.sprintf "%s.x"
  let get_y = Format.sprintf "%s.y"
  let get_z = Format.sprintf "%s.z"
  let get_w = Format.sprintf "%s.w"


  let circle radius uv =
    Format.sprintf "(length(%s) - %s)" uv radius

  let square radius uv =
    (* FIXME: Improve sharing of uv computation *)
    let x = uv |> get_x in
    let y = uv |> get_y in
    Format.sprintf "(max(abs(%s), abs(%s)) - %s)" x y radius


  let union = Format.sprintf "min(%s, %s)"
  let intersect = Format.sprintf "max(%s, %s)"
  let subtract = Format.sprintf "max(%s, -%s)"


  let move v sdf uv =
    sdf (Format.sprintf "(%s - %s)" uv v)

  let mirror_x sdf uv =
    (* FIXME: Improve sharing of uv computation *)
    let x = uv |> get_x in
    let y = uv |> get_y in
    sdf (Format.sprintf "vec2(abs(%s), %s)" x y)


  let mix ~bg ~fg ~shape =
    Format.sprintf "mix(%s, %s, step(0.0, %s))" fg bg shape

end


module MyScene (S : Sdf) = struct
  open S

  let f = float

  let scene =
    let* s1 = circle (f 0.1) |> move (vec2 (f 0.2) (f 0.0)) |> mirror_x in
    let* s2 = square (f 0.15) |> move (vec2 (f 0.1) (f 0.2)) in
    let shapeColor = vec3 (f 1.0) (f 1.0) (f 1.0) in

    pure (mix ~shape:(union s1 s2)
      ~bg:(vec3 (f 0.35) (f 0.45) (f 0.50))
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
