module Dom = Js_of_ocaml.Dom
module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let canvas_width = 400.0
let canvas_height = 400.0

let start (_ : (#Html.event as 'b) Js.t) : bool Js.t = begin
  (* Initialise canvas and 2D drawing context *)
  let canvas = Html.createCanvas Html.document in
  let ctx = canvas##getContext Html._2d_ in
  Dom.appendChild (Html.getElementById "tree") canvas;

  (* Set display size in CSS pixels *)
  canvas##.style##.width := Js.string ((canvas_width |> Float.to_int |> Int.to_string) ^ "px");
  canvas##.style##.height := Js.string ((canvas_height |> Float.to_int |> Int.to_string) ^ "px");

  (* Set the actual size in memory *)
  let scale = Html.window##.devicePixelRatio in
  canvas##.width := int_of_float (canvas_width *. scale);
  canvas##.height := int_of_float (canvas_height *. scale);

  (* Normalise the coordinate system to CSS pixels *)
  ctx##scale scale scale;

  (* Draw the scene *)
  begin
    ctx##save;

    (* Move to the center of the canvas *)
    ctx##translate (canvas_width *. 0.5) canvas_height;

    (* Draw the tree *)
    ctx |> Diagram.Canvas.run
      System.Binary_tree.(render (module Diagram.Canvas) (grow 5));

    ctx##restore;
  end;

  Js._false
end

let () =
  Html.window##.onload := Html.handler start
