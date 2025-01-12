(** {1 Typed shader language expressions} *)

module type Core = Shader_intf.Core
module type S = Shader_intf.S

(** Utilitiy functions derived from a shader module *)
module Make (L : Core) : S
  with type 'a repr = 'a L.repr
= struct

  include L

  module O = struct

    type 'a repr = 'a L.repr

    let ( !! ) = float

    let ( + ) = add
    let ( |+| ) = add_vec
    let ( |+ ) = add_scalar

    let ( - ) = sub
    let ( |-| ) = sub_vec
    let ( |- ) = sub_scalar

    let (  *  ) = mul
    let ( |*| ) = mul_vec
    let ( |* ) = mul_scalar

    let ( / ) = div
    let ( |/| ) = div_vec
    let ( |/ ) = div_scalar

    let ( % ) = mod_
    let ( |%| ) = mod_vec
    let ( |% ) = mod_scalar

    let ( .%{ } ) v c = get c v
    let ( .%{ }<- ) v c s = set c s v

  end

  open O

  let saturate s = clamp s ~min:!!0.0 ~max:!!1.0
  let saturate_vec v = clamp_scalar v ~min:!!0.0 ~max:!!1.0

  let zero2 = vec2 !!0.0 !!0.0
  let zero3 = vec3 !!0.0 !!0.0 !!0.0
  let zero4 = vec4 !!0.0 !!0.0 !!0.0 !!0.0

  let max_component2 v = max v.%{X} v.%{Y}
  let max_component3 v = max (max_component2 v) v.%{Z}
  let max_component4 v = max (max_component2 v) v.%{W}

  let min_component2 v = min v.%{X} v.%{Y}
  let min_component3 v = min (min_component2 v) v.%{Z}
  let min_component4 v = min (min_component2 v) v.%{W}


  let center_coords uv = uv |- !!0.5

  let corner_coords uv = uv |+ !!0.5

  let normalise_coords ~dimensions ~position =
    let uv = center_coords (position |/| dimensions) in
    (* Fix the aspect ratio of the x axis to remove warping *)
    let aspect = dimensions.%{X} / dimensions.%{Y} in
    uv.%{X} <- uv.%{X} * aspect


  module Component = struct

    (* TODO: Use a PPX instead?

      Eg. https://github.com/Octachron/tensority/blob/master/ppx/ppx_tensority.ml
    *)

    let x v = get X v
    let y v = get Y v
    let z v = get Z v
    let w v = get W v

    let set_x sx v = set X sx v
    let set_y sy v = set Y sy v
    let set_z sz v = set Z sz v
    let set_w sw v = set W sw v

    let xx v = get2 (X, X) v
    let xy v = get2 (X, Y) v
    let xz v = get2 (X, Z) v
    let xw v = get2 (X, W) v

    let yx v = get2 (Y, X) v
    let yy v = get2 (Y, Y) v
    let yz v = get2 (Y, Z) v
    let yw v = get2 (Y, W) v

    let zx v = get2 (Z, X) v
    let zy v = get2 (Z, Y) v
    let zz v = get2 (Z, Z) v
    let zw v = get2 (Z, W) v

    let wx v = get2 (W, X) v
    let wy v = get2 (W, Y) v
    let wz v = get2 (W, Z) v
    let ww v = get2 (W, W) v

    let xxx v = get3 (X, X, X) v
    let xxy v = get3 (X, X, Y) v
    let xxz v = get3 (X, X, Z) v
    let xxw v = get3 (X, X, W) v
    let xyx v = get3 (X, Y, X) v
    let xyy v = get3 (X, Y, Y) v
    let xyz v = get3 (X, Y, Z) v
    let xyw v = get3 (X, Y, W) v
    let xzx v = get3 (X, Z, X) v
    let xzy v = get3 (X, Z, Y) v
    let xzz v = get3 (X, Z, Z) v
    let xzw v = get3 (X, Z, W) v
    let xwx v = get3 (X, W, X) v
    let xwy v = get3 (X, W, Y) v
    let xwz v = get3 (X, W, Z) v
    let xww v = get3 (X, W, W) v

    let yxx v = get3 (Y, X, X) v
    let yxy v = get3 (Y, X, Y) v
    let yxz v = get3 (Y, X, Z) v
    let yxw v = get3 (Y, X, W) v
    let yyx v = get3 (Y, Y, X) v
    let yyy v = get3 (Y, Y, Y) v
    let yyz v = get3 (Y, Y, Z) v
    let yyw v = get3 (Y, Y, W) v
    let yzx v = get3 (Y, Z, X) v
    let yzy v = get3 (Y, Z, Y) v
    let yzz v = get3 (Y, Z, Z) v
    let yzw v = get3 (Y, Z, W) v
    let ywx v = get3 (Y, W, X) v
    let ywy v = get3 (Y, W, Y) v
    let ywz v = get3 (Y, W, Z) v
    let yww v = get3 (Y, W, W) v

    let zxx v = get3 (Z, X, X) v
    let zxy v = get3 (Z, X, Y) v
    let zxz v = get3 (Z, X, Z) v
    let zxw v = get3 (Z, X, W) v
    let zyx v = get3 (Z, Y, X) v
    let zyy v = get3 (Z, Y, Y) v
    let zyz v = get3 (Z, Y, Z) v
    let zyw v = get3 (Z, Y, W) v
    let zzx v = get3 (Z, Z, X) v
    let zzy v = get3 (Z, Z, Y) v
    let zzz v = get3 (Z, Z, Z) v
    let zzw v = get3 (Z, Z, W) v
    let zwx v = get3 (Z, W, X) v
    let zwy v = get3 (Z, W, Y) v
    let zwz v = get3 (Z, W, Z) v
    let zww v = get3 (Z, W, W) v

    let wxx v = get3 (W, X, X) v
    let wxy v = get3 (W, X, Y) v
    let wxz v = get3 (W, X, Z) v
    let wxw v = get3 (W, X, W) v
    let wyx v = get3 (W, Y, X) v
    let wyy v = get3 (W, Y, Y) v
    let wyz v = get3 (W, Y, Z) v
    let wyw v = get3 (W, Y, W) v
    let wzx v = get3 (W, Z, X) v
    let wzy v = get3 (W, Z, Y) v
    let wzz v = get3 (W, Z, Z) v
    let wzw v = get3 (W, Z, W) v
    let wwx v = get3 (W, W, X) v
    let wwy v = get3 (W, W, Y) v
    let wwz v = get3 (W, W, Z) v
    let www v = get3 (W, W, W) v

  end

end
