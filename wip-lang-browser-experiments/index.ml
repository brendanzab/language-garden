module Dom = Js_of_ocaml.Dom
module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let canvas_width = 400.0
let canvas_height = 400.0

let apex_diameter = 3.0
let branch_len = 6.0
let fork_angle = 45.0

let init_tree (iters : int) : Binary_tree.tree =
  let rec go iters tree =
    if iters < 0 then tree else
      (go [@tailcall]) (iters - 1) (Binary_tree.step tree)
  in
  go iters Binary_tree.axiom

let rec draw (ctx : Html.canvasRenderingContext2D Js.t) (tree : Binary_tree.tree) : unit =
  match tree with
  | Apex ->
      begin
        ctx##arc
          (* center_x *) (Js.float 0.0)
          (* center_y *) (Js.float 0.0)
          (* radius *) (Js.float (apex_diameter *. 0.5))
          (* theta1 *) (Js.float 0.0)
          (* theta2 *) (Js.float (2.0 *. Float.pi))
          (* ccw *) Js._false;
        ctx##fill;
      end

  | Fork (tree1, tree2) ->
      begin
        ctx##save;
        ctx##rotate (fork_angle *. Float.pi/.180.0);
        draw ctx tree1;
        ctx##restore;

        ctx##save;
        ctx##rotate (-.fork_angle *. Float.pi/.180.0);
        draw ctx tree2;
        ctx##restore;
      end

  | Branch tree ->
      begin
        ctx##save;

        ctx##beginPath;
        ctx##moveTo 0.0 0.0;
        ctx##lineTo 0.0 (-.branch_len);
        ctx##stroke;
        ctx##translate 0.0 (-.branch_len);

        draw ctx tree;

        ctx##restore;
      end

let start (_ : (#Html.event as 'b) Js.t) : bool Js.t = begin
  (* Initialise canvas and 2D drawing context *)
  let canvas = Html.createCanvas Html.document in
  let ctx = canvas##getContext Html._2d_ in
  Dom.appendChild (Html.getElementById "tree") canvas;

  (* Set display size in CSS pixels *)
  canvas##.style##.width := Js.string (Format.sprintf "%.0fpx" canvas_width);
  canvas##.style##.height := Js.string (Format.sprintf "%.0fpx" canvas_height);

  (* Set the actual size in memory *)
  let scale = Html.window##.devicePixelRatio in
  canvas##.width := int_of_float (canvas_width *. scale);
  canvas##.height := int_of_float (canvas_height *. scale);

  (* Normalise the coordinate system to CSS pixels *)
  ctx##scale scale scale;

  (* Render the tree *)
  begin
    ctx##save;

    ctx##translate (canvas_width *. 0.5) canvas_height;
    draw ctx (init_tree 5);

    ctx##restore;
  end;

  Js._false
end

let () = begin
  Html.window##.onload :=
    Html.handler start
end
