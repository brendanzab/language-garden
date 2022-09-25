type zero = Z
type 'n succ = Succ of 'n

type n0 = zero
type n1 = n0 succ
type n2 = n1 succ
type n3 = n2 succ
type n4 = n3 succ

(** Vectors indexed with a statically known size. This is not a very efficient
    representation, but we don’t actually use these for computation so it
    doesn’t really matter. *)
type (_, _) vec =
  | Nil : ('s, zero) vec
  | Cons : 's * ('s, 'n) vec -> ('s, 'n succ) vec

type 's vec1 = ('s , n1) vec
type 's vec2 = ('s , n2) vec
type 's vec3 = ('s , n3) vec
type 's vec4 = ('s , n4) vec

type ('s, 'n) vec1n = ('s , 'n succ) vec
type ('s, 'n) vec2n = ('s , 'n succ) vec1n
type ('s, 'n) vec3n = ('s , 'n succ) vec2n
type ('s, 'n) vec4n = ('s , 'n succ) vec3n

type 'n vecf = (float, 'n) vec

type vec1f = float vec1
type vec2f = float vec2
type vec3f = float vec3
type vec4f = float vec4

type 's mat2 = ('s vec2) vec2
type 's mat3 = ('s vec3) vec3
type 's mat4 = ('s vec4) vec4

type mat2f = float mat2
type mat3f = float mat3
type mat4f = float mat4


(** A language of linear algebra for a GLSL-like shader language *)
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

  (** Functions *)

  (* TODO: implement custom operators to make these a bit nicer to work with *)

  val neg : float repr -> float repr
  val neg_v : ('n vecf) repr -> ('n vecf) repr

  val add : float repr -> float repr -> float repr
  val add_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val add_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val sub : float repr -> float repr -> float repr
  val sub_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val sub_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val mul : float repr -> float repr -> float repr
  val mul_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mul_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val div : float repr -> float repr -> float repr
  val div_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val div_vs : ('n vecf) repr -> float repr -> ('n vecf) repr


  val abs : float repr -> float repr
  val abs_v : ('n vecf) repr -> ('n vecf) repr

  val clamp : float repr -> min:float repr -> max:float repr -> float repr
  val clamp_v : ('n vecf) repr -> min:('n vecf) repr -> max:('n vecf) repr -> ('n vecf) repr
  val clamp_vs : ('n vecf) repr -> min:float repr -> max:float repr -> ('n vecf) repr

  val length : ('n vecf) repr -> float repr

  val lerp : float repr -> float repr -> float repr -> float repr
  val lerp_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val lerp_vs : ('n vecf) repr -> ('n vecf) repr -> float repr -> ('n vecf) repr

  val max : float repr -> float repr -> float repr

  val min : float repr -> float repr -> float repr

  val mod_ : float repr -> float repr -> float repr
  val mod_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val mod_vs : ('n vecf) repr -> float repr -> ('n vecf) repr

  val round : float repr -> float repr
  val round_v : ('n vecf) repr -> ('n vecf) repr

  val step : float repr -> float repr -> float repr
  val step_v : ('n vecf) repr -> ('n vecf) repr -> ('n vecf) repr
  val step_vs : float repr -> ('n vecf) repr -> ('n vecf) repr

  (** Vector projections *)

  val x : ((float, 'n) vec1n) repr -> float repr
  val y : ((float, 'n) vec2n) repr -> float repr
  val z : ((float, 'n) vec3n) repr -> float repr
  val w : ((float, 'n) vec4n) repr -> float repr

  (** Vector swizzling *)

  val xx : ((float, 'n) vec1n) repr -> (float vec2) repr
  val xy : ((float, 'n) vec2n) repr -> (float vec2) repr
  val xz : ((float, 'n) vec3n) repr -> (float vec2) repr
  val xw : ((float, 'n) vec4n) repr -> (float vec2) repr

  val yx : ((float, 'n) vec2n) repr -> (float vec2) repr
  val yy : ((float, 'n) vec2n) repr -> (float vec2) repr
  val yz : ((float, 'n) vec3n) repr -> (float vec2) repr
  val yw : ((float, 'n) vec4n) repr -> (float vec2) repr

  val zx : ((float, 'n) vec3n) repr -> (float vec2) repr
  val zy : ((float, 'n) vec3n) repr -> (float vec2) repr
  val zz : ((float, 'n) vec3n) repr -> (float vec2) repr
  val zw : ((float, 'n) vec4n) repr -> (float vec2) repr

  val wx : ((float, 'n) vec4n) repr -> (float vec2) repr
  val wy : ((float, 'n) vec4n) repr -> (float vec2) repr
  val wz : ((float, 'n) vec4n) repr -> (float vec2) repr
  val ww : ((float, 'n) vec4n) repr -> (float vec2) repr

  (* TODO: More swizzles *)

end


(** A language for declaratively composing signed distance functions. *)
module type Sdf = sig

  (** The underlying representation of an SDF expression *)
  type 'a repr

  (** Signed distance to the boundary of a shape.

      - Negative values are inside the shape
      - Zero values are on the boundary of the shape
      - Positive values are outside the shape
  *)
  type dist = float repr

  (** A signed distanced function (SDF) describes a surface as a signed distance
      coordinate to the a signed distance to boundary of the surface. *)
  type 'n sdf = ((float, 'n) vec) repr -> dist

  (** A two-dimensional distance function *)
  type sdf2 = n2 sdf

  (** A three-dimensional distance function *)
  type sdf3 = n3 sdf


  (** {1 Primitive shape functions} *)

  (** Circle of a given radius *)
  val circle : float repr -> sdf2

  (** Square of a given radius *)
  val square : float repr -> sdf2


  (** {1 Operators for combining shapes} *)

  (** Union of two surfaces *)
  val union : dist -> dist -> dist

  (** The intersection of two surfaces *)
  val intersect : dist -> dist -> dist

  (** Substract one surface from another surface *)
  val subtract : dist -> dist -> dist


  (** {1 Operations on the domain of signed distance functions} *)

  (** Move a distance function by the supplied vector *)
  val move : ('n vecf) repr -> 'n sdf -> 'n sdf

  (** Uniformly scale a distance function by an amount *)
  val scale : float repr -> 'n sdf -> 'n sdf

  (** Reflect a copy of the distance function in each axis *)
  val reflect : 'n sdf -> 'n sdf

  (* TODO: Generalise reflect operations over dimension *)

  (** Reflect a copy of the distance function in the x axis *)
  val reflect2_x : sdf2 -> sdf2

  (** Reflect a copy of the distance function in the y axis *)
  val reflect2_y : sdf2 -> sdf2

  (** Reflect a copy of the distance function in the x axis *)
  val reflect3_x : sdf3 -> sdf3

  (** Reflect a copy of the distance function in the y axis *)
  val reflect3_y : sdf3 -> sdf3

  (** Reflect a copy of the distance function in the z axis *)
  val reflect3_z : sdf3 -> sdf3

  (** Repeat a distance function with the given spacing vector. The repetition
      cab be optionally limited to a bounding volume. *)
  val repeat : spacing:('n vecf) repr -> ?limit:('n vecf) repr -> 'n sdf -> 'n sdf


  (** {1 Compositing operations} *)

  (** Overlay a surface on top of a background color, painting it with a
      foreground color. *)
  val overlay : bg:vec3f repr -> fg:vec3f repr -> dist -> vec3f repr

end


module Sdf (M : Math) : Sdf

  with type 'a repr = 'a M.repr

= struct

  type 'a repr = 'a M.repr

  type dist = float repr
  type 'n sdf = ((float, 'n) vec) repr -> dist
  type sdf2 = n2 sdf
  type sdf3 = n3 sdf


  (** Based on the equation:

      [ x^2 + y^2 = r^2 ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let circle radius uv =
    M.sub (M.length uv) radius

  (** Based on the equation:

      [ max(abs(x), abs(y)) = r ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let square radius uv =
    let x_dist = uv |> M.x |> M.abs in
    let y_dist = uv |> M.y |> M.abs in
    M.sub (M.max x_dist y_dist) radius


  let union = M.min
  let intersect = M.max
  let subtract d1 d2 = M.min d1 (M.neg d2)


  let move v sdf uv =
    sdf (M.sub_v uv v)

  let scale factor sdf uv =
    M.mul (sdf (M.div_vs uv factor)) factor

  let reflect sdf uv =
    sdf (uv |> M.abs_v)

  let reflect2_x sdf uv =
    sdf (M.vec2
      (uv |> M.x |> M.abs)
      (uv |> M.y))

  let reflect2_y sdf uv =
    sdf (M.vec2
      (uv |> M.x)
      (uv |> M.y |> M.abs))

  let reflect3_x sdf uv =
    sdf (M.vec3
      (uv |> M.x |> M.abs)
      (uv |> M.y)
      (uv |> M.z))

  let reflect3_y sdf uv =
    sdf (M.vec3
      (uv |> M.x)
      (uv |> M.y |> M.abs)
      (uv |> M.z))

  let reflect3_z sdf uv =
    sdf (M.vec3
      (uv |> M.x)
      (uv |> M.y)
      (uv |> M.z |> M.abs))

  let repeat ~spacing ?limit sdf uv =
    match limit with
    | None ->
        let spacing_half = M.mul_vs spacing (M.float 0.5) in
        sdf (M.sub_v (M.mod_v (M.add_v uv spacing_half) spacing) spacing_half)
    | Some limit ->
        sdf (M.sub_v uv (M.mul_v spacing
          (M.clamp_v
            (M.round_v (M.div_v uv spacing))
            ~min:(M.neg_v limit)
            ~max:limit)))


  let overlay ~bg ~fg shape =
    M.lerp_vs fg bg (M.step (M.float 0.0) shape)

end


(** An environment that allows for the definition of shared computations. This
    is useful for compiling GLSL, where it would be preferrable to share the
    result of intermediate computations. *)
module GlslEnv = struct

  (** GLSL type *)
  type _ ty =
    | Float : float ty
    | Vec2 : vec2f ty
    | Vec3 : vec3f ty
    | Vec4 : vec4f ty
    | Mat2 : mat2f ty
    | Mat3 : mat3f ty
    | Mat4 : mat4f ty

  (** Compile a GLSL type to a string *)
  let string_of_ty : type a. a ty -> string =
    function
    | Float -> "float"
    | Vec2 -> "vec2"
    | Vec3 -> "vec3"
    | Vec4 -> "vec4"
    | Mat2 -> "mat2"
    | Mat3 -> "mat3"
    | Mat4 -> "mat4"

  type entry = {
    ty : string;
    def : string;
  }

  type locals = (string * entry) list

  type 'a m = locals -> 'a * locals

  let bind (state : 'a m) (f : 'a -> 'b m) : 'b m =
    fun locals ->
      let (expr', locals') = state locals in
      f expr' locals'

  let pure (x : 'a) : 'a m =
    fun locals -> (x, locals)

  (** Return the name of to a bound definition if it exists. *)
  let name_of_def def =
    List.find_map (fun (name, entry) -> if entry.def = def then Some name else None)

  (** Add a shared definition to the local environment. *)
  let add_local ty def : 'a m =
    (* FIXME: Make this more... type-safe? *)
    (* FIXME: Avoid names properly (including globals) *)
    fun locals ->
      match List.assoc_opt def locals with
      (* If the definition is the name of a currently bound local, return the
         local as a name without creating a new binding. *)
      | Some _ -> def, locals
      | None ->
          begin match name_of_def def locals with
          (* Only define a new definition if it has not already been bound. *)
          | Some name -> name, locals
          | None ->
              let name = Format.sprintf "t%i" (List.length locals) in
              let ty = string_of_ty ty in
              (name, (name, { ty; def }) :: locals)
          end

end


module Glsl : sig

  (** A typed GLSL expression *)
  type 'a expr

  (** Unsafely construct a GLSL expression from a string. *)
  val unsafe_expr : string -> 'a GlslEnv.ty -> 'a expr

  val string_of_expr : 'a expr -> string
  val ty_of_expr : 'a expr -> 'a GlslEnv.ty


  include Math with type 'a repr = ('a expr) GlslEnv.m

end = struct

  open GlslEnv
  open Control.Monad.Notation (GlslEnv)
  open Control.Monad.Util (GlslEnv)

  type 'a expr = {
    def : string;
    ty : 'a ty;
  }

  let unsafe_expr def ty = { def; ty }

  let string_of_expr e = e.def
  let ty_of_expr e = e.ty

  type 'a repr = ('a expr) GlslEnv.m

  let add_local_ann ty def =
    map (fun e -> { def = e; ty }) (add_local ty def)

  let pre ty op e =
    let* e = e in
    add_local_ann ty (Format.sprintf "%s%s" op e.def)

  let post ty op e =
    let* e = e in
    add_local_ann ty (Format.sprintf "%s%s" e.def op)

  let binop1 ty op e1 e2 =
    let* e1 = e1 in
    let* e2 = e2 in
    add_local_ann ty (Format.sprintf "%s %s %s" e1.def op e2.def)

  let call1 ty f e =
    let* e = e in
    add_local_ann ty (Format.sprintf "%s(%s)" f e.def)

  let call2 ty f e1 e2 =
    let* e1 = e1 in
    let* e2 = e2 in
    add_local_ann ty (Format.sprintf "%s(%s, %s)" f e1.def e2.def)

  let call3 ty f e1 e2 e3 =
    let* e1 = e1 in
    let* e2 = e2 in
    let* e3 = e3 in
    add_local_ann ty (Format.sprintf "%s(%s, %s, %s)" f e1.def e2.def e3.def)

  let call4 ty f e1 e2 e3 e4 =
    let* e1 = e1 in
    let* e2 = e2 in
    let* e3 = e3 in
    let* e4 = e4 in
    add_local_ann ty (Format.sprintf "%s(%s, %s, %s, %s)" f e1.def e2.def e3.def e4.def)


  (* TODO: Handle precedences more systematically *)

  let float x = pure { def = string_of_float x; ty = Float }

  let vec2 = call2 Vec2 "vec2"
  let vec3 = call3 Vec3 "vec3"
  let vec4 = call4 Vec4 "vec4"

  let mat2 = call2 Mat2 "mat2"
  let mat3 = call3 Mat3 "mat3"
  let mat4 = call4 Mat4 "mat4"

  let neg = pre Float "-"
  let neg_v e = let* e = e in pre e.ty "-" (pure e)
  let add = binop1 Float "+"
  let add_v e1 e2 = let* e1 = e1 in binop1 e1.ty "+" (pure e1) e2
  let add_vs e1 e2 = let* e1 = e1 in binop1 e1.ty "+" (pure e1) e2
  let sub = binop1 Float "-"
  let sub_v e1 e2 = let* e1 = e1 in binop1 e1.ty "-" (pure e1) e2
  let sub_vs e1 e2 = let* e1 = e1 in binop1 e1.ty "-" (pure e1) e2
  let mul = binop1 Float "*"
  let mul_v e1 e2 = let* e1 = e1 in binop1 e1.ty "*" (pure e1) e2
  let mul_vs e1 e2 = let* e1 = e1 in binop1 e1.ty "*" (pure e1) e2
  let div = binop1 Float "/"
  let div_v e1 e2 = let* e1 = e1 in binop1 e1.ty "/" (pure e1) e2
  let div_vs e1 e2 = let* e1 = e1 in binop1 e1.ty "/" (pure e1) e2
  let abs = call1 Float "abs"
  let abs_v e = let* e = e in call1 e.ty "abs" (pure e)
  let clamp e ~min ~max = call3 Float "clamp" e min max
  let clamp_v e ~min ~max = let* e = e in call3 e.ty "clamp" (pure e) min max
  let clamp_vs e ~min ~max = let* e = e in call3 e.ty "clamp" (pure e) min max
  let length e = call1 Float "length" e
  let lerp = call3 Float "mix"
  let lerp_v e1 e2 e3 = let* e1 = e1 in call3 e1.ty "mix" (pure e1) e2 e3
  let lerp_vs e1 e2 e3 = let* e1 = e1 in call3 e1.ty "mix" (pure e1) e2 e3
  let max = call2 Float "max"
  let min = call2 Float "min"
  let mod_ = call2 Float "mod"
  let mod_v e1 e2 = let* e1 = e1 in call2 e1.ty "mod" (pure e1) e2
  let mod_vs e1 e2 = let* e1 = e1 in call2 e1.ty "mod" (pure e1) e2
  let round = call1 Float "round"
  let round_v e = let* e = e in call1 e.ty "round" (pure e)
  let step = call2 Float "step"
  let step_v e1 e2 = let* e1 = e1 in call2 e1.ty "step" (pure e1) e2
  let step_vs e1 e2 = let* e2 = e2 in call2 e2.ty "step" e1 (pure e2)

  let x e = post Float ".x" e
  let y e = post Float ".y" e
  let z e = post Float ".z" e
  let w e = post Float ".w" e

  let xx e = post Vec2 ".xx" e
  let xy e = post Vec2 ".xy" e
  let xz e = post Vec2 ".xz" e
  let xw e = post Vec2 ".xw" e

  let yx e = post Vec2 ".yx" e
  let yy e = post Vec2 ".yy" e
  let yz e = post Vec2 ".yz" e
  let yw e = post Vec2 ".yw" e

  let zx e = post Vec2 ".zx" e
  let zy e = post Vec2 ".zy" e
  let zz e = post Vec2 ".zz" e
  let zw e = post Vec2 ".zw" e

  let wx e = post Vec2 ".wx" e
  let wy e = post Vec2 ".wy" e
  let wz e = post Vec2 ".wz" e
  let ww e = post Vec2 ".ww" e

end


module MyScene (M : Math) = struct

  open Sdf (M)

  let f = M.float
  let vec2f x y = M.vec2 (f x) (f y)
  let vec3f x y z = M.vec3 (f x) (f y) (f z)

  (** Vertical gradient *)
  let background (uv : vec2f repr) : vec3f repr =
    (* Remap UV coordinates from (-0.5, 0.5) to (0, 1) *)
    let uv = M.add_vs uv (f 0.5) in

    let bottom_color = vec3f 0.35 0.45 0.50 in
    let top_color = vec3f 0.85 0.85 0.70 in
    let amount = M.add (uv |> M.y) (M.mul (uv |> M.x) (f 0.2)) in

    M.lerp_vs bottom_color top_color amount

  let scene (uv : vec2f repr) : vec3f repr =
    let bg = background uv in
    let s1 = circle (f 0.05) |> repeat ~spacing:(vec2f 0.2 0.2) in
    let s2 = square (f 0.15) |> move (vec2f 0.2 0.2) |> reflect in

    let shapeColor = vec3f 1.0 1.0 1.0 in

    overlay ~bg ~fg:shapeColor (union (s1 uv) (s2 uv))

end


let () =
  let module S = MyScene (Glsl) in

  let (color, locals) = S.scene (GlslEnv.pure (Glsl.unsafe_expr "uv" Vec2 )) [] in

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
  locals |> List.rev |> List.iter (fun (name, GlslEnv.{ ty; def }) ->
    Format.printf "  %s %s = %s;\n" ty name def);
  Format.printf "\n";
  Format.printf "  // Compute the colour for this UV coordinate.\n";
  Format.printf "  vec3 color = %s;\n" (Glsl.string_of_expr color);
  Format.printf "\n";
  Format.printf "  // Output to screen\n";
  Format.printf "  fragColor = vec4(color,1.0);\n";
  Format.printf "}\n";

  ()
