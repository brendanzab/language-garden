open Shader_types

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
  | Any_expr : 'a expr -> any_expr


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
    List.find_map (fun (name, Any_expr expr') ->
      (* If the definition is the name of a currently bound local, return the
          local as a name without creating a new binding. *)
      if expr.def = name then Some { expr with def = name }
      (* Only define a new definition if it has not already been bound. *)
      else if equal_expr expr expr' then Some { expr with def = name }
      else None)

  let define (type a) (expr : a expr) (locals : t) : a expr * t =
    match find_expr expr locals with
    | Some expr -> expr, locals
    | None ->
        let name = fresh_name locals in
        { expr with def = name }, (name, Any_expr expr) :: locals

end


module Env = struct

  include Control.Monad.State.Make (Locals)

  let define_local expr =
    embed (Locals.define expr)

end


open Env
open Env.O


type 'a repr = ('a expr) Env.t


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

let abs s = call1 Float "abs" s
let abs_vec v = bind v (fun v -> call1 v.ty "abs" (pure v))
let clamp s ~min ~max = call3 Float "clamp" s min max
let clamp_vec v ~min ~max = bind v (fun v -> call3 v.ty "clamp" (pure v) min max)
let clamp_scalar v ~min ~max = bind v (fun v -> call3 v.ty "clamp" (pure v) min max)
let cos a = call1 Float "cos" a
let cos_vec v = bind v (fun v -> call1 v.ty "cos" (pure v))
let dot v1 v2 = call2 Float "dot" v1 v2
let length v = call1 Float "length" v
let lerp s1 s2 s3 = call3 Float "mix" s1 s2 s3
let lerp_vec v1 v2 v3 = bind v1 (fun v1 -> call3 v1.ty "mix" (pure v1) v2 v3)
let lerp_scalar v s1 s2 = bind v (fun v -> call3 v.ty "mix" (pure v) s1 s2)
let max s1 s2 = call2 Float "max" s1 s2
let max_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "max" (pure v1) v2)
let min s1 s2 = call2 Float "min" s1 s2
let min_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "min" (pure v1) v2)
let pow s = call2 Float "pow" s
let pow_vec v1 v2 = bind v1 (fun v1 -> call2 v1.ty "pow" (pure v1) v2)
let round s = call1 Float "round" s
let round_vec v = bind v (fun v -> call1 v.ty "round" (pure v))
let sin a = call1 Float "sin" a
let sin_vec v = bind v (fun v -> call1 v.ty "sin" (pure v))
let smooth_step ~lower ~upper s = call3 Float "smoothstep" lower upper s
let smooth_step_vec ~lower ~upper v = bind v (fun v -> call3 v.ty "smoothstep" lower upper (pure v))
let smooth_step_scalar ~lower ~upper v = bind v (fun v -> call3 v.ty "smoothstep" lower upper (pure v))
let sqrt a = call1 Float "sqrt" a
let sqrt_vec v = bind v (fun v -> call1 v.ty "sqrt" (pure v))
let step ~edge s = call2 Float "step" edge s
let step_vec ~edge v = bind v (fun v -> call2 v.ty "step" edge (pure v))
let step_scalar ~edge v = bind v (fun v -> call2 v.ty "step" edge (pure v))
let tan a = call1 Float "tan" a
let tan_vec v = bind v (fun v -> call1 v.ty "tan" (pure v))


(** The name of a vector component *)
let field_name (type n) : n component -> string =
  function
  | X -> "x"
  | Y -> "y"
  | Z -> "z"
  | W -> "w"

let get (type n) (c : n component) (v : (n vecf) repr) : float repr =
  post Float (Format.sprintf ".%s" (field_name c)) v

let get2 (type n) (c1, c2 : n component * n component) (v : (n vecf) repr) : vec2f repr =
  post Vec2 (Format.sprintf ".%s%s" (field_name c1) (field_name c2)) v

let get3 (type n) (c1, c2, c3 : n component * n component * n component) (v : (n vecf) repr) : vec3f repr =
  post Vec3 (Format.sprintf ".%s%s%s" (field_name c1) (field_name c2) (field_name c3)) v

let get4 (type n) (c1, c2, c3, c4 : n component * n component * n component * n component) (v : (n vecf) repr) : vec4f repr =
  post Vec4 (Format.sprintf ".%s%s%s%s" (field_name c1) (field_name c2) (field_name c3) (field_name c4)) v

let set (type n) (c : n component) (s : float repr) (v : (n vecf) repr) : (n vecf) repr =
  let go (type n) (c : n component) s (v : (n vecf) expr) : (n vecf) repr =
    match c, v with
    | X, { ty = Vec2; _ } -> vec2 s (pure v |> get Y)
    | X, { ty = Vec3; _ } -> vec3 s (pure v |> get Y) (pure v |> get Z)
    | X, { ty = Vec4; _ } -> vec4 s (pure v |> get Y) (pure v |> get Z) (pure v |> get W)
    | Y, { ty = Vec2; _ } -> vec2 (pure v |> get X) s
    | Y, { ty = Vec3; _ } -> vec3 (pure v |> get X) s (pure v |> get Z)
    | Y, { ty = Vec4; _ } -> vec4 (pure v |> get X) s (pure v |> get Z) (pure v |> get W)
    | Z, { ty = Vec3; _ } -> vec3 (pure v |> get X) (pure v |> get Y) s
    | Z, { ty = Vec4; _ } -> vec4 (pure v |> get X) (pure v |> get Y) s (pure v |> get W)
    | W, { ty = Vec4; _ } -> vec4 (pure v |> get X) (pure v |> get Y) (pure v |> get Z) s
  in
  bind v (go c s)


let map_vec f v =
  let go (type n) f (v : (n vecf) expr) : (n vecf) repr =
    match v with
    | { ty = Vec2; _ } -> vec2 (pure v |> get X |> f) (pure v |> get Y |> f)
    | { ty = Vec3; _ } -> vec3 (pure v |> get X |> f) (pure v |> get Y |> f) (pure v |> get Z |> f)
    | { ty = Vec4; _ } -> vec4 (pure v |> get X |> f) (pure v |> get Y |> f) (pure v |> get Z |> f) (pure v |> get W |> f)
  in
  bind v (go f)

let fold_left_vec f acc v =
  let go (type a n) f (acc : a repr) (v : (n vecf) expr) : a repr =
    match v with
    | { ty = Vec2; _ } ->
        let acc = pure v |> get Y |> f acc in
        let acc = pure v |> get X |> f acc in
        acc
    | { ty = Vec3; _ } ->
        let acc = pure v |> get Z |> f acc in
        let acc = pure v |> get Y |> f acc in
        let acc = pure v |> get X |> f acc in
        acc
    | { ty = Vec4; _ } ->
        let acc = pure v |> get W |> f acc in
        let acc = pure v |> get Z |> f acc in
        let acc = pure v |> get Y |> f acc in
        let acc = pure v |> get X |> f acc in
        acc
  in
  bind v (go f acc)


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

  let compile_image_shader (shader : image_shader) =
    let (color, locals) =
      Env.run (shader uniforms frag_coord) Locals.empty in

    Format.printf "// The main entrypoint of the shader.\n";
    Format.printf "//\n";
    Format.printf "// Copy and paste this into https://www.shadertoy.com/new to see the output.\n";
    Format.printf "void mainImage(out vec4 fragColor, in vec2 fragCoord) {\n";
    locals |> Locals.iter (fun (name, Any_expr expr) ->
      Format.printf "  %s %s = %s;\n" (string_of_ty expr.ty) name expr.def);
    Format.printf "\n";
    Format.printf "  // Set the color of the current pixel\n";
    Format.printf "  fragColor = vec4(%s, 1.0);\n" (string_of_expr color);
    Format.printf "}\n"

end
