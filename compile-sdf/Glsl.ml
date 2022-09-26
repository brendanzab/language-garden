open MathTypes

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


module Env = struct

  type locals = (string * any_expr) list

  let empty_locals = []
  let iter_locals f locals = locals |> List.rev |> List.iter f

  type 'a m = locals -> 'a * locals

  let bind (state : 'a m) (f : 'a -> 'b m) : 'b m =
    fun locals ->
      let (expr', locals') = state locals in
      f expr' locals'

  let pure (x : 'a) : 'a m =
    fun locals -> (x, locals)


  let run locals m =
    m locals


  let fresh_name locals =
    (* FIXME: Avoid names properly (including globals) *)
    Format.sprintf "t%i" (List.length locals)

  (** If possible, returns and expression that refers to an existing local the
      matches the supplied expression. *)
  let lookup_expr expr =
    List.find_map (fun (name, AnyExpr expr') ->
      (* If the definition is the name of a currently bound local, return the
          local as a name without creating a new binding. *)
      if expr.def = name then Some { expr with def = name }
      (* Only define a new definition if it has not already been bound. *)
      else if equal_expr expr expr' then Some { expr with def = name }
      else None)

  (** Add a shared definition to the local environment, avoiding the
      introduction of common sub-expressions. *)
  let define_local (expr : 'a expr) : ('a expr) m =
    fun locals ->
      match lookup_expr expr locals with
      | Some expr -> expr, locals
      | None ->
          let name = fresh_name locals in
          { expr with def = name }, (name, AnyExpr expr) :: locals

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
let neg_vec e = bind e (fun e -> pre e.ty "-" (pure e))
let add = binop1 Float "+"
let add_vec e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "+" (pure e1) e2)
let add_scalar e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "+" (pure e1) e2)
let sub = binop1 Float "-"
let sub_vec e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "-" (pure e1) e2)
let sub_scalar e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "-" (pure e1) e2)
let mul = binop1 Float "*"
let mul_vec e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "*" (pure e1) e2)
let mul_scalar e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "*" (pure e1) e2)
let div = binop1 Float "/"
let div_vec e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "/" (pure e1) e2)
let div_scalar e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "/" (pure e1) e2)
let mod_ = call2 Float "mod"
let mod_vec e1 e2 = bind e1 (fun e1 -> call2 e1.ty "mod" (pure e1) e2)
let mod_scalar e1 e2 = bind e1 (fun e1 -> call2 e1.ty "mod" (pure e1) e2)

let abs = call1 Float "abs"
let abs_vec e = bind e (fun e -> call1 e.ty "abs" (pure e))
let clamp e ~min ~max = call3 Float "clamp" e min max
let clamp_vec e ~min ~max = bind e (fun e -> call3 e.ty "clamp" (pure e) min max)
let clamp_scalar e ~min ~max = bind e (fun e -> call3 e.ty "clamp" (pure e) min max)
let cos = call1 Float "cos"
let cos_vec e = bind e (fun e -> call1 e.ty "cos" (pure e))
let dot e1 e2 = call2 Float "dot" e1 e2
let length e = call1 Float "length" e
let lerp = call3 Float "mix"
let lerp_vec e1 e2 e3 = bind e1 (fun e1 -> call3 e1.ty "mix" (pure e1) e2 e3)
let lerp_scalar e1 e2 e3 = bind e1 (fun e1 -> call3 e1.ty "mix" (pure e1) e2 e3)
let max = call2 Float "max"
let max_vec e1 e2 = bind e1 (fun e1 -> call2 e1.ty "max" (pure e1) e2)
let min = call2 Float "min"
let min_vec e1 e2 = bind e1 (fun e1 -> call2 e1.ty "min" (pure e1) e2)
let round = call1 Float "round"
let round_vec e = bind e (fun e -> call1 e.ty "round" (pure e))
let sin = call1 Float "sin"
let sin_vec e = bind e (fun e -> call1 e.ty "sin" (pure e))
let sqrt = call1 Float "sqrt"
let sqrt_vec e = bind e (fun e -> call1 e.ty "sqrt" (pure e))
let step = call2 Float "step"
let step_vec e1 e2 = bind e1 (fun e1 -> call2 e1.ty "step" (pure e1) e2)
let step_scalar e1 e2 = bind e2 (fun e2 -> call2 e2.ty "step" e1 (pure e2))
let tan = call1 Float "tan"
let tan_vec e = bind e (fun e -> call1 e.ty "tan" (pure e))

let x v = post Float ".x" v
let y v = post Float ".y" v
let z v = post Float ".z" v
let w v = post Float ".w" v

(* The following dimension-polymorphic setters are challenging to implement
   because higher-rank anonymous functions are difficult, if not impossible to
   express in OCaml as far as I can tell. Instead we implement local functions
   with explicit type signatures. *)

let set_x s v =
  let set_x (type n) s : ((float, n) vec1n) expr -> ((float, n) vec1n) repr =
    function
    | { ty = Vec2; _ } as v -> vec2 (pure v |> x) s
    | { ty = Vec3; _ } as v -> vec3 (pure v |> x) s (pure v |> z)
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) s (pure v |> z) (pure v |> w)
  in
  bind v (set_x s)

let set_y s v =
  let set_y (type n) s : ((float, n) vec2n) expr -> ((float, n) vec2n) repr =
    function
    | { ty = Vec2; _ } as v -> vec2 (pure v |> x) s
    | { ty = Vec3; _ } as v -> vec3 (pure v |> x) s (pure v |> z)
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) s (pure v |> z) (pure v |> w)
  in
  bind v (set_y s)

let set_z s v =
  let set_z (type n) s : ((float, n) vec3n) expr -> ((float, n) vec3n) repr =
    function
    | { ty = Vec3; _ } as v -> vec3 (pure v |> x) (pure v |> y) s
    | { ty = Vec4; _ } as v -> vec4 (pure v |> x) (pure v |> y) s (pure v |> w)
  in
  bind v (set_z s)

let set_w s v =
  let set_w (type n) s : ((float, n) vec4n) expr -> ((float, n) vec4n) repr =
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
    let (color, locals) = Env.run Env.empty_locals (shader uniforms frag_coord) in

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
