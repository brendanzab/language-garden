open ShaderTypes

(** GLSL type *)
type _ ty =
  | Float : float ty
  | Vec2 : vec2f ty
  | Vec3 : vec3f ty
  | Vec4 : vec4f ty
  | Mat2 : mat2f ty
  | Mat3 : mat3f ty
  | Mat4 : mat4f ty

let equal_ty (type a b) (t1 : a ty) (t2 : b ty) : bool =
  match t1, t2 with
  | Float, Float -> true
  | Vec2, Vec2 -> true
  | Vec3, Vec3 -> true
  | Vec4, Vec4 -> true
  | Mat2, Mat2 -> true
  | Mat3, Mat3 -> true
  | Mat4, Mat4 -> true
  | _, _ -> false

let string_of_ty : type a. a ty -> string =
  function
  | Float -> "float"
  | Vec2 -> "vec2"
  | Vec3 -> "vec3"
  | Vec4 -> "vec4"
  | Mat2 -> "mat2"
  | Mat3 -> "mat3"
  | Mat4 -> "mat4"


type 'a expr = {
  def : string;
  ty : 'a ty;
}

let unsafe_expr def ty = { def; ty }

let equal_expr e1 e2 =
  e1.def = e2.def && equal_ty e1.ty e2.ty

let string_of_expr e = e.def

let ty_of_expr e = e.ty

type any_expr =
  | AnyExpr : 'a expr -> any_expr


module Locals = struct

  type t = (string * any_expr) list

  let empty = []

  let iter f locals =
    locals |> List.rev |> List.iter f

  let fresh_name locals =
    (* FIXME: Avoid names properly (including globals) *)
    Format.sprintf "t%i" (List.length locals)

  (** If possible, returns and expression that refers to an existing local the
      matches the supplied expression. *)
  let find_expr expr =
    List.find_map (fun (name, AnyExpr expr') ->
      (* If the definition is the name of a currently bound local, return the
          local as a name without creating a new binding. *)
      if expr.def = name then Some { expr with def = name }
      (* Only define a new definition if it has not already been bound. *)
      else if equal_expr expr expr' then Some { expr with def = name }
      else None)

  let define (expr : 'a expr) (locals : t) : 'a expr * t =
    match find_expr expr locals with
    | Some expr -> expr, locals
    | None ->
        let name = fresh_name locals in
        { expr with def = name }, (name, AnyExpr expr) :: locals

end


module Env = struct

  include Control.Monad.State (Locals)

  let define_local expr =
    embed (Locals.define expr)

end


open Env
open Control.Monad.Notation (Env)
open Control.Monad.Util (Env)


type 'a repr = ('a expr) Env.m


(* TODO: Figure out how to make this module cleaner. At the moment juggling
         expressions is a bit clunky and painful! *)

let pre ty op e =
  let* e = e in
  define_local { ty; def = Format.sprintf "%s%s" op e.def }

let post ty op e =
  let* e = e in
  define_local { ty; def = Format.sprintf "%s%s" e.def op }

let binop1 ty op e1 e2 =
  let* e1 = e1 in
  let* e2 = e2 in
  define_local { ty; def = Format.sprintf "%s %s %s" e1.def op e2.def }

let call1 ty f e =
  let* e = e in
  define_local { ty; def = Format.sprintf "%s(%s)" f e.def }

let call2 ty f e1 e2 =
  let* e1 = e1 in
  let* e2 = e2 in
  define_local { ty; def = Format.sprintf "%s(%s, %s)" f e1.def e2.def }

let call3 ty f e1 e2 e3 =
  let* e1 = e1 in
  let* e2 = e2 in
  let* e3 = e3 in
  define_local { ty; def = Format.sprintf "%s(%s, %s, %s)" f e1.def e2.def e3.def }

let call4 ty f e1 e2 e3 e4 =
  let* e1 = e1 in
  let* e2 = e2 in
  let* e3 = e3 in
  let* e4 = e4 in
  define_local { ty; def = Format.sprintf "%s(%s, %s, %s, %s)" f e1.def e2.def e3.def e4.def }

let float x = pure { def = string_of_float x; ty = Float }

let vec2 = call2 Vec2 "vec2"
let vec3 = call3 Vec3 "vec3"
let vec4 = call4 Vec4 "vec4"

let mat2 = call2 Mat2 "mat2"
let mat3 = call3 Mat3 "mat3"
let mat4 = call4 Mat4 "mat4"

let neg = pre Float "-"
let neg_vec v = bind v (fun v -> pre v.ty "-" (pure v))
let add = binop1 Float "+"
let add_vec v1 v2 = bind v1 (fun v1 -> binop1 v1.ty "+" (pure v1) v2)
let add_scalar v s = bind v (fun v -> binop1 v.ty "+" (pure v) s)
let sub = binop1 Float "-"
let sub_vec v1 v2 = bind v1 (fun v1 -> binop1 v1.ty "-" (pure v1) v2)
let sub_scalar v s = bind v (fun v -> binop1 v.ty "-" (pure v) s)
let mul = binop1 Float "*"
let mul_vec v1 v2 = bind v1 (fun v1 -> binop1 v1.ty "*" (pure v1) v2)
let mul_scalar v s = bind v (fun v -> binop1 v.ty "*" (pure v) s)
let div = binop1 Float "/"
let div_vec v1 v2 = bind v1 (fun v1 -> binop1 v1.ty "/" (pure v1) v2)
let div_scalar v s = bind v (fun v -> binop1 v.ty "/" (pure v) s)
let mod_ = call2 Float "mod"
let mod_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "mod" (pure v1) v2)
let mod_scalar v s = bind v (fun v -> call2 v.ty "mod" (pure v) s)

let abs = call1 Float "abs"
let abs_vec v = bind v (fun v -> call1 v.ty "abs" (pure v))
let clamp e ~min ~max = call3 Float "clamp" e min max
let clamp_vec v ~min ~max = bind v (fun v -> call3 v.ty "clamp" (pure v) min max)
let clamp_scalar v ~min ~max = bind v (fun v -> call3 v.ty "clamp" (pure v) min max)
let cos = call1 Float "cos"
let cos_vec v = bind v (fun v -> call1 v.ty "cos" (pure v))
let dot e1 e2 = call2 Float "dot" e1 e2
let length e = call1 Float "length" e
let lerp = call3 Float "mix"
let lerp_vec v1 v2 v3 = bind v1 (fun v1 -> call3 v1.ty "mix" (pure v1) v2 v3)
let lerp_scalar v s1 s2 = bind v (fun v -> call3 v.ty "mix" (pure v) s1 s2)
let max = call2 Float "max"
let max_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "max" (pure v1) v2)
let min = call2 Float "min"
let min_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "min" (pure v1) v2)
let round = call1 Float "round"
let round_vec v = bind v (fun v -> call1 v.ty "round" (pure v))
let sin = call1 Float "sin"
let sin_vec v = bind v (fun v -> call1 v.ty "sin" (pure v))
let sqrt = call1 Float "sqrt"
let sqrt_vec v = bind v (fun v -> call1 v.ty "sqrt" (pure v))
let step = call2 Float "step"
let step_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "step" (pure v1) v2)
let step_scalar s v = bind v (fun v -> call2 v.ty "step" s (pure v))
let tan = call1 Float "tan"
let tan_vec v = bind v (fun v -> call1 v.ty "tan" (pure v))

let x v = post Float ".x" v
let y v = post Float ".y" v
let z v = post Float ".z" v
let w v = post Float ".w" v

(* The following dimension-polymorphic setters are challenging to implement
   because higher-rank anonymous functions are difficult, if not impossible to
   express in OCaml as far as I can tell. Instead we implement local functions
   with explicit type signatures. *)

let set_x s v =
  let set_x (type n) s : n vec_ge1f expr -> n vec_ge1f repr =
    function
    | { ty = Vec2; _ } as v -> vec2 (pure v |> x) s
    | { ty = Vec3; _ } as v -> vec3 (pure v |> x) s (pure v |> z)
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) s (pure v |> z) (pure v |> w)
  in
  bind v (set_x s)

let set_y s v =
  let set_y (type n) s : n vec_ge2f expr -> n vec_ge2f repr =
    function
    | { ty = Vec2; _ } as v -> vec2 (pure v |> x) s
    | { ty = Vec3; _ } as v -> vec3 (pure v |> x) s (pure v |> z)
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) s (pure v |> z) (pure v |> w)
  in
  bind v (set_y s)

let set_z s v =
  let set_z (type n) s : n vec_ge3f expr -> n vec_ge3f repr =
    function
    | { ty = Vec3; _ } as v -> vec3 (pure v |> x) (pure v |> y) s
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) (pure v |> y) s (pure v |> w)
  in
  bind v (set_z s)

let set_w s v =
  let set_w (type n) s : n vec_ge4f expr -> n vec_ge4f repr =
    function
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) (pure v |> y) (pure v |> z) s
  in
  bind v (set_w s)

let xx v = post Vec2 ".xx" v
let xy v = post Vec2 ".xy" v
let xz v = post Vec2 ".xz" v
let xw v = post Vec2 ".xw" v

let yx v = post Vec2 ".yx" v
let yy v = post Vec2 ".yy" v
let yz v = post Vec2 ".yz" v
let yw v = post Vec2 ".yw" v

let zx v = post Vec2 ".zx" v
let zy v = post Vec2 ".zy" v
let zz v = post Vec2 ".zz" v
let zw v = post Vec2 ".zw" v

let wx v = post Vec2 ".wx" v
let wy v = post Vec2 ".wy" v
let wz v = post Vec2 ".wz" v
let ww v = post Vec2 ".ww" v

let xxx v = post Vec3 ".xxx" v
let xxy v = post Vec3 ".xxy" v
let xxz v = post Vec3 ".xxz" v
let xxw v = post Vec3 ".xxw" v
let xyx v = post Vec3 ".xyx" v
let xyy v = post Vec3 ".xyy" v
let xyz v = post Vec3 ".xyz" v
let xyw v = post Vec3 ".xyw" v
let xzx v = post Vec3 ".xzx" v
let xzy v = post Vec3 ".xzy" v
let xzz v = post Vec3 ".xzz" v
let xzw v = post Vec3 ".xzw" v
let xwx v = post Vec3 ".xwx" v
let xwy v = post Vec3 ".xwy" v
let xwz v = post Vec3 ".xwz" v
let xww v = post Vec3 ".xww" v

let yxx v = post Vec3 ".yxx" v
let yxy v = post Vec3 ".yxy" v
let yxz v = post Vec3 ".yxz" v
let yxw v = post Vec3 ".yxw" v
let yyx v = post Vec3 ".yyx" v
let yyy v = post Vec3 ".yyy" v
let yyz v = post Vec3 ".yyz" v
let yyw v = post Vec3 ".yyw" v
let yzx v = post Vec3 ".yzx" v
let yzy v = post Vec3 ".yzy" v
let yzz v = post Vec3 ".yzz" v
let yzw v = post Vec3 ".yzw" v
let ywx v = post Vec3 ".ywx" v
let ywy v = post Vec3 ".ywy" v
let ywz v = post Vec3 ".ywz" v
let yww v = post Vec3 ".yww" v

let zxx v = post Vec3 ".zxx" v
let zxy v = post Vec3 ".zxy" v
let zxz v = post Vec3 ".zxz" v
let zxw v = post Vec3 ".zxw" v
let zyx v = post Vec3 ".zyx" v
let zyy v = post Vec3 ".zyy" v
let zyz v = post Vec3 ".zyz" v
let zyw v = post Vec3 ".zyw" v
let zzx v = post Vec3 ".zzx" v
let zzy v = post Vec3 ".zzy" v
let zzz v = post Vec3 ".zzz" v
let zzw v = post Vec3 ".zzw" v
let zwx v = post Vec3 ".zwx" v
let zwy v = post Vec3 ".zwy" v
let zwz v = post Vec3 ".zwz" v
let zww v = post Vec3 ".zww" v

let wxx v = post Vec3 ".wxx" v
let wxy v = post Vec3 ".wxy" v
let wxz v = post Vec3 ".wxz" v
let wxw v = post Vec3 ".wxw" v
let wyx v = post Vec3 ".wyx" v
let wyy v = post Vec3 ".wyy" v
let wyz v = post Vec3 ".wyz" v
let wyw v = post Vec3 ".wyw" v
let wzx v = post Vec3 ".wzx" v
let wzy v = post Vec3 ".wzy" v
let wzz v = post Vec3 ".wzz" v
let wzw v = post Vec3 ".wzw" v
let wwx v = post Vec3 ".wwx" v
let wwy v = post Vec3 ".wwy" v
let wwz v = post Vec3 ".wwz" v
let www v = post Vec3 ".www" v


module Shadertoy = struct

  type uniforms = {
    resolution : vec3f repr;
    time : float repr;
    time_delta : float repr;
    frame : float repr;
    mouse : vec4f repr;
    date : vec4f repr;
    sample_rate : float repr;
  }

  let uniforms = {
    resolution = Env.pure (unsafe_expr "iResolution" Vec3);
    time = Env.pure (unsafe_expr "iTime" Float);
    time_delta = Env.pure (unsafe_expr "iTimeDelta" Float);
    frame = Env.pure (unsafe_expr "iFrame" Float);
    mouse = Env.pure (unsafe_expr "mouse" Vec4);
    date = Env.pure (unsafe_expr "date" Vec4);
    sample_rate = Env.pure (unsafe_expr "sample_rate" Float);
  }

  type image_shader = uniforms -> vec2f repr -> vec3f repr

  let frag_coord = Env.pure (unsafe_expr "fragCoord" Vec2)

  let compile_local (name, AnyExpr { def; ty }) =
    Format.sprintf "  %s %s = %s;" (string_of_ty ty) name def

  let compile_image_shader (shader : image_shader) =
    let (color, locals) =
      Env.run (shader uniforms frag_coord) Locals.empty in

    String.concat "\n" ([
      "// The main entrypoint of the shader.";
      "//";
      "// Copy and paste this into https://www.shadertoy.com/new to see the output.";
      "void mainImage(out vec4 fragColor, in vec2 fragCoord) {";
    ] @ List.map compile_local (List.rev locals) @ [
      "";
      "  // Set the color of the current pixel";
      Format.sprintf "  fragColor = vec4(%s, 1.0);" (string_of_expr color);
      "}";
    ])

end
