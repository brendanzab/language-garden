open ShaderTypes


type 'a repr = 'a

let float s = s

let vec2 s1 s2 = Cons (s1, Cons (s2, Nil))
let vec3 s1 s2 s3 = Cons (s1, Cons (s2, Cons (s3, Nil)))
let vec4 s1 s2 s3 s4 = Cons (s1, Cons (s2, Cons (s3, Cons (s4, Nil))))

let mat2 v1 v2 = Cons (v1, Cons (v2, Nil))
let mat3 v1 v2 v3 = Cons (v1, Cons (v2, Cons (v3, Nil)))
let mat4 v1 v2 v3 v4 = Cons (v1, Cons (v2, Cons (v3, Cons (v4, Nil))))

let neg s = -.s
let neg_vec v = map_vec neg v

let add = (+.)
let add_vec v1 v2 = zip_with_vec add v1 v2
let add_scalar v s = map_vec (add s) v

let sub = (-.)
let sub_vec v1 v2 = zip_with_vec sub v1 v2
let sub_scalar v s = map_vec (sub s) v

let mul = ( *. )
let mul_vec v1 v2 = zip_with_vec mul v1 v2
let mul_scalar v s = map_vec (mul s) v

let div = (/.)
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
(* See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/clamp.xhtml *)
let clamp s ~min:smin ~max:smax = min (max s smin) smax
let clamp_vec v ~min ~max = zip_with3_vec (fun s min max -> clamp s ~min ~max) v min max
let clamp_scalar v ~min ~max = map_vec (clamp ~min ~max) v
let cos = Float.cos
let cos_vec v = map_vec cos v
let dot v1 v2 = fold_left_vec (+.) 0.0 (zip_with_vec ( *.) v1 v2)
let length2 v = dot v v
let length v = sqrt (length2 v)
let lerp s1 s2 a = s1 *. (1.0 -. a) +. s2 *. a
let lerp_vec v1 v2 a = zip_with3_vec lerp v1 v2 a
let lerp_scalar v1 v2 a = zip_with_vec (fun s1 s2 -> lerp s1 s2 a) v1 v2
let round = Float.round
let round_vec v = map_vec round v
let sin = Float.sin
let sin_vec v = map_vec sin v
let sqrt = Float.sqrt
let sqrt_vec v = map_vec sqrt v
(* See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/step.xhtml *)
let step edge s = if s < edge then 0.0 else 1.0
let step_vec edge v = zip_with_vec step edge v
let step_scalar edge v = map_vec (step edge) v
let tan = Float.tan
let tan_vec v = map_vec tan v

let x (Cons (s, _)) = s
let y (Cons (_, (Cons (s, _)))) = s
let z (Cons (_, (Cons (_, (Cons (s, _)))))) = s
let w (Cons (_, (Cons (_, (Cons (_, (Cons (s, _)))))))) = s

let set_x sx (Cons (_, v)) = Cons (sx, v)
let set_y sy (Cons (sx, Cons (_, v))) = Cons (sx, Cons (sy, v))
let set_z sz (Cons (sx, Cons (sy, Cons (_, v)))) = Cons (sx, Cons (sy, Cons (sz, v)))
let set_w sw (Cons (sx, Cons (sy, Cons (sz, Cons (_, v))))) = Cons (sx, Cons (sy, Cons (sz, Cons (sw, v))))

let xx v = vec2 (x v) (x v)
let xy v = vec2 (x v) (y v)
let xz v = vec2 (x v) (z v)
let xw v = vec2 (x v) (w v)

let yx v = vec2 (y v) (x v)
let yy v = vec2 (y v) (y v)
let yz v = vec2 (y v) (z v)
let yw v = vec2 (y v) (w v)

let zx v = vec2 (z v) (x v)
let zy v = vec2 (z v) (y v)
let zz v = vec2 (z v) (z v)
let zw v = vec2 (z v) (w v)

let wx v = vec2 (w v) (x v)
let wy v = vec2 (w v) (y v)
let wz v = vec2 (w v) (z v)
let ww v = vec2 (w v) (w v)

let xxx v = vec3 (x v) (x v) (x v)
let xxy v = vec3 (x v) (x v) (y v)
let xxz v = vec3 (x v) (x v) (z v)
let xxw v = vec3 (x v) (x v) (w v)
let xyx v = vec3 (x v) (y v) (x v)
let xyy v = vec3 (x v) (y v) (y v)
let xyz v = vec3 (x v) (y v) (z v)
let xyw v = vec3 (x v) (y v) (w v)
let xzx v = vec3 (x v) (z v) (x v)
let xzy v = vec3 (x v) (z v) (y v)
let xzz v = vec3 (x v) (z v) (z v)
let xzw v = vec3 (x v) (z v) (w v)
let xwx v = vec3 (x v) (w v) (x v)
let xwy v = vec3 (x v) (w v) (y v)
let xwz v = vec3 (x v) (w v) (z v)
let xww v = vec3 (x v) (w v) (w v)

let yxx v = vec3 (y v) (x v) (x v)
let yxy v = vec3 (y v) (x v) (y v)
let yxz v = vec3 (y v) (x v) (z v)
let yxw v = vec3 (y v) (x v) (w v)
let yyx v = vec3 (y v) (y v) (x v)
let yyy v = vec3 (y v) (y v) (y v)
let yyz v = vec3 (y v) (y v) (z v)
let yyw v = vec3 (y v) (y v) (w v)
let yzx v = vec3 (y v) (z v) (x v)
let yzy v = vec3 (y v) (z v) (y v)
let yzz v = vec3 (y v) (z v) (z v)
let yzw v = vec3 (y v) (z v) (w v)
let ywx v = vec3 (y v) (w v) (x v)
let ywy v = vec3 (y v) (w v) (y v)
let ywz v = vec3 (y v) (w v) (z v)
let yww v = vec3 (y v) (w v) (w v)

let zxx v = vec3 (z v) (x v) (x v)
let zxy v = vec3 (z v) (x v) (y v)
let zxz v = vec3 (z v) (x v) (z v)
let zxw v = vec3 (z v) (x v) (w v)
let zyx v = vec3 (z v) (y v) (x v)
let zyy v = vec3 (z v) (y v) (y v)
let zyz v = vec3 (z v) (y v) (z v)
let zyw v = vec3 (z v) (y v) (w v)
let zzx v = vec3 (z v) (z v) (x v)
let zzy v = vec3 (z v) (z v) (y v)
let zzz v = vec3 (z v) (z v) (z v)
let zzw v = vec3 (z v) (z v) (w v)
let zwx v = vec3 (z v) (w v) (x v)
let zwy v = vec3 (z v) (w v) (y v)
let zwz v = vec3 (z v) (w v) (z v)
let zww v = vec3 (z v) (w v) (w v)

let wxx v = vec3 (w v) (x v) (x v)
let wxy v = vec3 (w v) (x v) (y v)
let wxz v = vec3 (w v) (x v) (z v)
let wxw v = vec3 (w v) (x v) (w v)
let wyx v = vec3 (w v) (y v) (x v)
let wyy v = vec3 (w v) (y v) (y v)
let wyz v = vec3 (w v) (y v) (z v)
let wyw v = vec3 (w v) (y v) (w v)
let wzx v = vec3 (w v) (z v) (x v)
let wzy v = vec3 (w v) (z v) (y v)
let wzz v = vec3 (w v) (z v) (z v)
let wzw v = vec3 (w v) (z v) (w v)
let wwx v = vec3 (w v) (w v) (x v)
let wwy v = vec3 (w v) (w v) (y v)
let wwz v = vec3 (w v) (w v) (z v)
let www v = vec3 (w v) (w v) (w v)


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

  y_coords |> Seq.iter (fun fy ->
    x_coords |> Seq.iter (fun fx ->
      let color = shader (vec2 fx fy) in (* sloooowwww... *)
      let color = mul_scalar color 255.0 |> clamp_scalar ~min:0.0 ~max:255.0 in
      Printf.printf "%.0f %.0f %.0f\n" (x color) (y color) (z color)))
