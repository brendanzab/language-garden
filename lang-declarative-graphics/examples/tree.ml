open Declarative_graphics_core

let apex_diameter = 3.0
let fork_angle = 45.0 *. Float.pi /. 180.0
let branch_length = 6.0

let tree (type t) (module D : Diagram.S with type t = t) ~(iters : int) : t =
  let rec  apex =
    D.circle ~diameter:apex_diameter

  and branch ~(iters : int) : t =
    let len = branch_length *. Float.pow 2.0 (float_of_int iters) in
    D.stack [
      D.line (0.0, 0.0) (0.0, -.len)
        |> D.stroke `solid;
      D.translate_y (-.len)
        (if iters <= 0 then apex else fork ~iters);
    ]

  and fork ~(iters : int) =
    D.stack [
      branch ~iters:(iters - 1) |> D.rotate ~radians:(+.fork_angle);
      branch ~iters:(iters - 1) |> D.rotate ~radians:(-.fork_angle);
    ]
  in

  branch ~iters

let () =
  let (width, height) = (400.0, 400.0) in

  tree (module Svg_diagram) ~iters:5
    |> Svg_diagram.translate (width /. 2.0, height)
    |> Svg_diagram.run ~view_box:(0.0, 0.0, width, height)
    |> print_string
