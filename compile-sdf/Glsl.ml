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


module Env = struct

  type entry = {
    def : string;
    ty : string;
  }

  type locals = (string * entry) list

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

  (** Find the name a pre-existing definition in the environment, if it exists. *)
  let name_of_entry { def; ty } =
    List.find_map (fun (name, entry) ->
      (* If the definition is the name of a currently bound local, return the
          local as a name without creating a new binding. *)
      if def = name then Some name
      (* Only define a new definition if it has not already been bound. *)
      else if entry.def = def && entry.ty == ty then Some name
      else None)

  (** Add a shared definition to the local environment, avoiding the
      introduction of common sub-expressions. *)
  let add_local ty def =
    (* FIXME: Make this more... type-safe? *)
    fun locals ->
      let ty = string_of_ty ty in
      match name_of_entry { def; ty } locals with
      | Some name -> name, locals
      | None ->
          let name = fresh_name locals in
          (name, (name, { def; ty }) :: locals)

end


open Env
open Control.Monad.Notation (Env)
open Control.Monad.Util (Env)

type 'a expr = {
  def : string;
  ty : 'a ty;
}

let unsafe_expr def ty = { def; ty }

let string_of_expr e = e.def
let ty_of_expr e = e.ty

type 'a repr = ('a expr) Env.m


(* TODO: Figure out how to make this module cleaner. At the moment juggling
          expressions is a bit clunky and painful! *)

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

let float x = pure { def = string_of_float x; ty = Float }

let vec2 = call2 Vec2 "vec2"
let vec3 = call3 Vec3 "vec3"
let vec4 = call4 Vec4 "vec4"

let mat2 = call2 Mat2 "mat2"
let mat3 = call3 Mat3 "mat3"
let mat4 = call4 Mat4 "mat4"

let neg = pre Float "-"
let neg_v e = bind e (fun e -> pre e.ty "-" (pure e))
let add = binop1 Float "+"
let add_v e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "+" (pure e1) e2)
let add_vs e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "+" (pure e1) e2)
let sub = binop1 Float "-"
let sub_v e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "-" (pure e1) e2)
let sub_vs e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "-" (pure e1) e2)
let mul = binop1 Float "*"
let mul_v e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "*" (pure e1) e2)
let mul_vs e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "*" (pure e1) e2)
let div = binop1 Float "/"
let div_v e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "/" (pure e1) e2)
let div_vs e1 e2 = bind e1 (fun e1 -> binop1 e1.ty "/" (pure e1) e2)
let abs = call1 Float "abs"
let abs_v e = bind e (fun e -> call1 e.ty "abs" (pure e))
let clamp e ~min ~max = call3 Float "clamp" e min max
let clamp_v e ~min ~max = bind e (fun e -> call3 e.ty "clamp" (pure e) min max)
let clamp_vs e ~min ~max = bind e (fun e -> call3 e.ty "clamp" (pure e) min max)
let length e = call1 Float "length" e
let lerp = call3 Float "mix"
let lerp_v e1 e2 e3 = bind e1 (fun e1 -> call3 e1.ty "mix" (pure e1) e2 e3)
let lerp_vs e1 e2 e3 = bind e1 (fun e1 -> call3 e1.ty "mix" (pure e1) e2 e3)
let max = call2 Float "max"
let min = call2 Float "min"
let mod_ = call2 Float "mod"
let mod_v e1 e2 = bind e1 (fun e1 -> call2 e1.ty "mod" (pure e1) e2)
let mod_vs e1 e2 = bind e1 (fun e1 -> call2 e1.ty "mod" (pure e1) e2)
let round = call1 Float "round"
let round_v e = bind e (fun e -> call1 e.ty "round" (pure e))
let step = call2 Float "step"
let step_v e1 e2 = bind e1 (fun e1 -> call2 e1.ty "step" (pure e1) e2)
let step_vs e1 e2 = bind e2 (fun e2 -> call2 e2.ty "step" e1 (pure e2))

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
