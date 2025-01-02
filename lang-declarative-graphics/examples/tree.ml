open Declarative_graphics_core

let tree (type t) (module D : Diagram.S with type t = t) ~(theta : float) ~(width:float) ~(height:float) : t =
  let rec branch (len : float) (n : int) : t =
    D.stack [
      D.line (0.0, 0.0) (0.0, -.len)
        |> D.stroke `solid;
      D.translate_y (-.len)
        (if n <= 0 then apex else fork (len *. 0.66) (n - 1))
    ]
  and apex =
    D.circle ~diameter:3.0
  and fork (len : float) (n : int) =
    D.stack [
      branch len n |> D.rotate ~radians:(+.theta);
      branch len n |> D.rotate ~radians:(-.theta);
    ]
  in

  branch 150.0 4
    |> D.translate (width /. 2.0, height)

let () =
  let theta = Float.pi /. 6.0 in
  let (width, height) = (800.0, 500.0) in

  tree (module Svg_diagram) ~theta ~width ~height
    |> Svg_diagram.run ~view_box:(0.0, 0.0, width, height)
    |> print_string
