open Shader_types


type 'a repr = 'a

let float s = s

let vec2 s1 s2 = vec2 s1 s2
let vec3 s1 s2 s3 = vec3 s1 s2 s3
let vec4 s1 s2 s3 s4 = vec4 s1 s2 s3 s4

let mat2 v1 v2 = vec2 v1 v2
let mat3 v1 v2 v3 = vec3 v1 v2 v3
let mat4 v1 v2 v3 v4 = vec4 v1 v2 v3 v4

let neg s = -.s
let neg_vec v = map_vec neg v

let add = ( +. )
let add_vec v1 v2 = zip_with_vec add v1 v2
let add_scalar v s = map_vec (add s) v

let sub = ( -. )
let sub_vec v1 v2 = zip_with_vec sub v1 v2
let sub_scalar v s = map_vec (sub s) v

let mul = ( *. )
let mul_vec v1 v2 = zip_with_vec mul v1 v2
let mul_scalar v s = map_vec (mul s) v

let div = ( /. )
let div_vec v1 v2 = zip_with_vec div v1 v2
let div_scalar v s = map_vec (div s) v

(* See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/mod.xhtml *)
let mod_ s1 s2 = s1 -. s2 *. Float.floor (s1 /. s2)
let mod_vec v1 v2 = zip_with_vec mod_ v1 v2
let mod_scalar v s = map_vec (mod_ s) v

let abs = Float.abs
let abs_vec v = map_vec abs v

let max = Float.max
let max_vec v1 v2 = zip_with_vec max v1 v2

let min = Float.min
let min_vec v1 v2 = zip_with_vec min v1 v2

let pow = Float.pow
let pow_vec v1 v2 = zip_with_vec pow v1 v2

(* See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/clamp.xhtml *)
let clamp s ~min:smin ~max:smax = min (max s smin) smax

let clamp_vec v ~min ~max =
   zip_with3_vec (fun s min max -> clamp s ~min ~max) v min max

let clamp_scalar v ~min ~max = map_vec (clamp ~min ~max) v

let cos = Float.cos
let cos_vec v = map_vec cos v

let dot v1 v2 = fold_left_vec ( +. ) 0.0 (zip_with_vec ( *. ) v1 v2)

let length2 v = dot v v
let length v = sqrt (length2 v)

let lerp s1 s2 a = s1 *. (1.0 -. a) +. s2 *. a
let lerp_vec v1 v2 a = zip_with3_vec lerp v1 v2 a
let lerp_scalar v1 v2 a = zip_with_vec (fun s1 s2 -> lerp s1 s2 a) v1 v2

let round = Float.round
let round_vec v = map_vec round v

let sin = Float.sin
let sin_vec v = map_vec sin v

(* See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/smoothstep.xhtml *)
let smooth_step ~lower ~upper s =
   let t = clamp ((s -. lower) /. (upper -. lower)) ~min:0.0 ~max:1.0 in
   t *. t *. (3.0 -. 2.0 *. t) *. t

let smooth_step_vec ~lower ~upper v =
   zip_with3_vec (fun lower upper v -> smooth_step ~lower ~upper v) lower upper v

let smooth_step_scalar ~lower ~upper v =
   map_vec (fun v -> smooth_step ~lower ~upper v) v

let sqrt = Float.sqrt
let sqrt_vec v = map_vec sqrt v

(* See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/step.xhtml *)
let step ~edge s = if s < edge then 0.0 else 1.0
let step_vec ~edge v = zip_with_vec (fun edge v -> step ~edge v) edge v
let step_scalar ~edge v = map_vec (fun v -> step ~edge v) v

let tan = Float.tan
let tan_vec v = map_vec tan v


let map_vec  = map_vec
let fold_left_vec  = fold_left_vec


let get = get
let get2 = get2
let get3 = get3
let get4 = get4
let set = set


type image_shader = vec2f repr -> vec3f repr

(* See https://en.wikipedia.org/wiki/Netpbm#PPM_example *)
let render_ppm ~width ~height (shader : image_shader) =
  Printf.printf "P3\n";                   (* this is a RGB color image in ASCII *)
  Printf.printf "%i %i\n" width height;   (* the width and height of the image in pixels *)
  Printf.printf "255\n";                  (* maximum value for each color *)

  (* Use the default OpenGL coordinate system, which starts at the
     bottom-left corner of the screen:

     y
     ▲  ┌────────────────┐
     │  │                │
     │  │                │
     │  │                │
     │  └────────────────┘
     └────────────────────► x

     https://registry.khronos.org/OpenGL-Refpages/gl4/html/gl_FragCoord.xhtml
  *)

  let x_coords = Seq.iterate (fun x -> x -. 1.0) (float_of_int width) |> Seq.take width in
  let y_coords = Seq.iterate (fun y -> y +. 1.0) 0.0 |> Seq.take height in

  (* To be honest, I'm not sure why the above sequences of coordinates work. I
     would have assumed that we should increment the x coordinates starting from
     zero, and decrement the y coordinates starting from the height, but this
     results in an image that is flipped in both axes compared to the image
     rendered in Shadertoy. *)

  (* TODO: In Rust a library like Rayon could be used to run this computation in
     parallel. Does an equivalent library exist for OCaml? This might require
     the parallelism support added in OCaml 5. *)
  y_coords |> Seq.iter (fun fy ->
    x_coords |> Seq.iter (fun fx ->
      let color = shader (vec2 fx fy) in
      let color = mul_scalar color 255.0 |> clamp_scalar ~min:0.0 ~max:255.0 in

      Printf.printf "%.0f %.0f %.0f\n"
         (color |> get X)
         (color |> get Y)
         (color |> get Z)))
