type ('s, 'rest) vec1n = ('s * 'rest)
type ('s, 'rest) vec2n = ('s * ('s, 'rest) vec1n)
type ('s, 'rest) vec3n = ('s * ('s, 'rest) vec2n)
type ('s, 'rest) vec4n = ('s * ('s, 'rest) vec3n)

type 's vec2 = ('s, unit) vec2n
type 's vec3 = ('s, unit) vec3n
type 's vec4 = ('s, unit) vec4n

type vec2f = float vec2
type vec3f = float vec3
type vec4f = float vec4

type 's mat2 = (('s, unit) vec2n, unit) vec2n
type 's mat3 = (('s, unit) vec3n, unit) vec3n
type 's mat4 = (('s, unit) vec4n, unit) vec4n

type mat2f = float mat2
type mat3f = float mat3
type mat4f = float mat4


(** A language of linear algebra *)
module type Math = sig

  type 'a repr

  (** Literals *)

  val float : float -> float repr

  val vec2 : float repr -> float repr -> (float vec2) repr
  val vec3 : float repr -> float repr -> float repr -> (float vec3) repr
  val vec4 : float repr -> float repr -> float repr -> float repr -> (float vec4) repr

  val mat2 : (float vec2) repr -> (float vec2) repr -> (float mat2) repr
  val mat3 : (float vec3) repr -> (float vec3) repr -> (float vec3) repr -> (float mat3) repr
  val mat4 : (float vec4) repr -> (float vec4) repr -> (float vec4) repr -> (float vec4) repr -> (float mat4) repr

  (** Vector projections *)

  val x : ((float, 'rest) vec1n) repr -> float repr
  val y : ((float, 'rest) vec2n) repr -> float repr
  val z : ((float, 'rest) vec3n) repr -> float repr
  val w : ((float, 'rest) vec4n) repr -> float repr

  (** Vector swizzling *)

  val xx : ((float, 'rest) vec1n) repr -> (float vec2) repr
  val xy : ((float, 'rest) vec2n) repr -> (float vec2) repr
  val xz : ((float, 'rest) vec3n) repr -> (float vec2) repr
  val xw : ((float, 'rest) vec4n) repr -> (float vec2) repr

  val yx : ((float, 'rest) vec2n) repr -> (float vec2) repr
  val yy : ((float, 'rest) vec2n) repr -> (float vec2) repr
  val yz : ((float, 'rest) vec3n) repr -> (float vec2) repr
  val yw : ((float, 'rest) vec4n) repr -> (float vec2) repr

  val zx : ((float, 'rest) vec3n) repr -> (float vec2) repr
  val zy : ((float, 'rest) vec3n) repr -> (float vec2) repr
  val zz : ((float, 'rest) vec3n) repr -> (float vec2) repr
  val zw : ((float, 'rest) vec4n) repr -> (float vec2) repr

  val wx : ((float, 'rest) vec4n) repr -> (float vec2) repr
  val wy : ((float, 'rest) vec4n) repr -> (float vec2) repr
  val wz : ((float, 'rest) vec4n) repr -> (float vec2) repr
  val ww : ((float, 'rest) vec4n) repr -> (float vec2) repr

end


(** A language of signed distance functions *)
module type Sdf = sig

  type 'a repr

  module Math : Math with type 'a repr = 'a repr

  (** An environment that provides access to a UV coordinate *)
  module UvEnv : sig

    (* TODO: Is this monadic? The signatures look a bit weird with the use of `repr`! *)

    (** A computation with access to a UV coordinate *)
    type 'a m

    (** Run a computation that depends on the current UV coordinate *)
    val bind : 'a m -> ('a repr -> 'b m) -> 'b m

    (** Embed a pure value that does not depend on the current UV coordinate *)
    val pure : 'a repr -> 'a m

    (** Get the UV coordinate in the environment *)
    val ask : (vec2f repr) m

    (** Run a computation with an initial UV coordinate *)
    val run : vec2f repr -> 'a m -> 'a repr

  end

  (** A signed distance function from a 2D point to a distance to the boundary
      of a surface. Points outside the surface return positive values, and
      points inside the surface return negative values. *)
  type sdf2 = float UvEnv.m

  val circle : float repr -> sdf2
  val square : float repr -> sdf2

  val union : float repr -> float repr -> float repr
  val intersect : float repr -> float repr -> float repr
  val subtract : float repr -> float repr -> float repr

  val move : vec2f repr -> 'a UvEnv.m -> 'a UvEnv.m
  val scale : float repr -> 'a UvEnv.m -> 'a UvEnv.m
  val mirror_x : 'a UvEnv.m -> 'a UvEnv.m
  val mirror_y : 'a UvEnv.m -> 'a UvEnv.m
  val mirror_xy : 'a UvEnv.m -> 'a UvEnv.m
  val repeat : spacing:vec2f repr -> 'a UvEnv.m -> 'a UvEnv.m
  val repeat_limit : spacing:vec2f repr -> limit:vec2f repr -> 'a UvEnv.m -> 'a UvEnv.m

  val overlay : bg:(vec3f repr) -> fg:(vec3f repr) -> float repr -> float repr

end


(** An environment that allows for the definition of shared computations. This
    is useful for compiling GLSL, where it would be preferrable to share the
    result of intermediate computations. *)
module GlslEnv = struct

  type locals = (string * string) list

  type 'a m = locals -> 'a * locals

  let bind (state : 'a m) (f : 'a -> 'b m) : 'b m =
    fun locals ->
      let (expr', locals') = state locals in
      f expr' locals'

  let pure (x : 'a) : 'a m =
    fun locals -> (x, locals)


  let add_local ty x : 'a m =
    (* FIXME: Avoid names properly (including globals) *)
    fun locals ->
      ("t" ^ string_of_int (List.length locals), (ty, x) :: locals)

end


module GlslMath : Math with type 'a repr = string GlslEnv.m = struct

  open GlslEnv
  open Control.Monad.Notation (GlslEnv)
  open Control.Monad.Util (GlslEnv)

  type 'a repr = string GlslEnv.m

  let float x = pure (string_of_float x)

  let vec2 = map2 (Format.sprintf "vec2(%s, %s)")
  let vec3 = map3 (Format.sprintf "vec3(%s, %s, %s)")
  let vec4 = map4 (Format.sprintf "vec4(%s, %s, %s, %s)")

  let mat2 = map2 (Format.sprintf "mat2(%s, %s)")
  let mat3 = map3 (Format.sprintf "mat3(%s, %s, %s)")
  let mat4 = map4 (Format.sprintf "mat4(%s, %s, %s, %s)")

  let x = map (Format.sprintf "%s.x")
  let y = map (Format.sprintf "%s.y")
  let z = map (Format.sprintf "%s.z")
  let w = map (Format.sprintf "%s.w")

  let xx  = map (Format.sprintf "%s.xx")
  let xy  = map (Format.sprintf "%s.xy")
  let xz  = map (Format.sprintf "%s.xz")
  let xw  = map (Format.sprintf "%s.xw")

  let yx  = map (Format.sprintf "%s.yx")
  let yy  = map (Format.sprintf "%s.yy")
  let yz  = map (Format.sprintf "%s.yz")
  let yw  = map (Format.sprintf "%s.yw")

  let zx  = map (Format.sprintf "%s.zx")
  let zy  = map (Format.sprintf "%s.zy")
  let zz  = map (Format.sprintf "%s.zz")
  let zw  = map (Format.sprintf "%s.zw")

  let wx  = map (Format.sprintf "%s.wx")
  let wy  = map (Format.sprintf "%s.wy")
  let wz  = map (Format.sprintf "%s.wz")
  let ww  = map (Format.sprintf "%s.ww")

end


(** Compile the signed distance functions to GLSL *)
module GlslSdf : Sdf with type 'a repr = string GlslEnv.m = struct

  open GlslEnv
  open Control.Monad.Notation (GlslEnv)
  open Control.Monad.Util (GlslEnv)


  type 'a repr = string GlslEnv.m

  module Math = GlslMath

  module UvEnv = struct

    type 'a m = vec2f repr -> 'a repr

    let bind sdf f = fun uv -> f (sdf uv) uv
    let pure x = fun _ -> x

    let ask : (vec2f repr) m = fun uv -> uv
    let run (uv : vec2f repr) (sdf : 'a m) : 'a repr = sdf uv

  end

  type sdf2 = float UvEnv.m


  (* NOTE: We try to reduce the number of parentheses in the compiled output by
     only wrapping expressions when we don’t know if they are atomic or not.
     For example if an expression is refers to a local binding, then we don’t
     need parentheses, as variable names are atomic expressions.*)

  (* TODO: Handle precedences more systematically *)


  let circle radius uv =
    map2 (Format.sprintf "length(%s) - (%s)") uv radius

  let square radius uv =
    let* uv = bind uv (add_local "vec2") in
    map3 (Format.sprintf "(max(abs(%s), abs(%s)) - %s)")
      (pure uv |> Math.x)
      (pure uv |> Math.y)
      radius


  let union = map2 (Format.sprintf "min(%s, %s)")
  let intersect = map2 (Format.sprintf "max(%s, %s)")
  let subtract = map2 (Format.sprintf "max(%s, -%s)")


  let move v sdf uv =
    sdf (map2 (Format.sprintf "%s - %s") uv v)

  let scale s sdf uv =
    let* uv = uv in
    let* s = bind s (add_local "float") in
    map2 (Format.sprintf "(%s) * %s") (sdf (pure (Format.sprintf "(%s) / %s" uv s))) (pure s)

  let mirror_x sdf uv =
    let* uv = bind uv (add_local "vec2") in
    sdf (map2 (Format.sprintf "vec2(abs(%s), %s)")
      (pure uv |> Math.x)
      (pure uv |> Math.y))

  let mirror_y sdf uv =
    let* uv = bind uv (add_local "vec2") in
    sdf (map2 (Format.sprintf "vec2(%s, abs(%s))")
      (pure uv |> Math.x)
      (pure uv |> Math.y))

  let mirror_xy sdf uv =
    sdf (map (Format.sprintf "vec2(abs(%s))") uv)

  let repeat ~spacing sdf uv =
    let* uv = uv in
    let* spacing = bind spacing (add_local "vec2") in
    sdf (pure (Format.sprintf "mod((%s) + 0.5 * %s, %s) - 0.5 * %s" uv spacing spacing spacing))

  let repeat_limit ~spacing ~limit sdf uv =
    let* uv = bind uv (add_local "vec2") in
    let* spacing = bind spacing (add_local "vec2") in
    let* limit = bind limit (add_local "vec2") in
    sdf (pure (Format.sprintf "%s - %s * clamp(round(%s / %s), -%s, %s)" uv spacing uv spacing limit limit))


  let overlay ~bg ~fg shape =
    map3 (Format.sprintf "mix(%s, %s, step(0.0, %s))") fg bg shape

end


module MyScene (S : Sdf) = struct

  open S

  let f = Math.float
  let vec2f x y = Math.vec2 (f x) (f y)
  let vec3f x y z = Math.vec3 (f x) (f y) (f z)

  let (let*) = UvEnv.bind
  let pure = UvEnv.pure

  let scene : sdf2 =
    let* s1 = circle (f 0.05) |> repeat ~spacing:(vec2f 0.2 0.2) in
    let* s2 = square (f 0.15) |> move (vec2f 0.1 0.2) in

    let shapeColor = vec3f 1.0 1.0 1.0 in
    let backgroundColor = vec3f 0.35 0.45 0.50 in

    pure (overlay ~bg:backgroundColor ~fg:shapeColor (union s1 s2))

end


let () =
  let module S = MyScene (GlslSdf) in

  let (color, locals) =
    GlslSdf.UvEnv.run (GlslEnv.pure "uv") S.scene [] in

  Format.printf "// The main entrypoint of the shader.\n";
  Format.printf "//\n";
  Format.printf "// Copy and paste this into %s to see the output.\n" "https://www.shadertoy.com/new";
  Format.printf "void mainImage(out vec4 fragColor, in vec2 fragCoord) {\n";
  Format.printf "  // Normalise the UV coordinates to <-0.5,0.5>\n";
  Format.printf "  vec2 uv = fragCoord / iResolution.xy - 0.5;\n";
  Format.printf "  // Fix the aspect ratio of the x axis\n";
  Format.printf "  uv.x *= iResolution.x / iResolution.y;\n";
  Format.printf "\n";
  Format.printf "  // Local bindings\n";
  locals |> List.rev |> List.iteri (fun i (ty, def) ->
    Format.printf "  %s t%i = %s;\n" ty i def);
  Format.printf "\n";
  Format.printf "  // Compute the colour for this UV coordinate.\n";
  Format.printf "  vec3 color = %s;\n" color;
  Format.printf "\n";
  Format.printf "  // Output to screen\n";
  Format.printf "  fragColor = vec4(color,1.0);\n";
  Format.printf "}\n";

  ()
