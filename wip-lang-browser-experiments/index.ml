module Dom = Js_of_ocaml.Dom
module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

(** Declarative diagramming API *)
module Diagram = struct

  type vec2 = float * float

  module type S = sig

    type t

    val circle : diameter:float -> t
    val line : vec2 -> vec2 -> t

    val set_stroke : bool -> t -> t
    val set_fill : bool -> t -> t

    val overlay : t list -> t
    val rotate : radians:float -> t -> t
    val translate : vec2 -> t -> t

  end

  (** A declarative version of the Canvas API *)
  module Canvas : sig

    type t

    include S with type t := t

    val run : t -> Html.canvasRenderingContext2D Js.t -> unit

  end = struct

    type t = fill:bool -> stroke:bool -> Html.canvasRenderingContext2D Js.t -> unit

    let arc ~center:(center_x, center_y) ~radius ~theta1 ~theta2 ~ccw =
      fun ~fill ~stroke ctx ->
        ctx##arc
          (* center_x *) (Js.float center_x)
          (* center_y *) (Js.float center_y)
          (* radius *) (Js.float radius)
          (* theta1 *) (Js.float theta1)
          (* theta2 *) (Js.float theta2)
          (* ccw *) (Js.bool ccw);
        if fill then ctx##fill;
        if stroke then ctx##stroke;
        ()

    let circle ~diameter =
      arc
        ~center:(0.0, 0.0)
        ~radius:(diameter *. 0.5)
        ~theta1:0.0
        ~theta2:(2.0 *. Float.pi)
        ~ccw:false

    let line (x1, y1) (x2, y2) =
      fun ~fill:_ ~stroke ctx ->
        if stroke then begin
          ctx##beginPath;
          ctx##moveTo x1 y1;
          ctx##lineTo x2 y2;
          ctx##stroke;
        end

    let set_stroke value dia =
      fun ~fill ~stroke:_ ctx ->
        dia ~fill ~stroke:value ctx

    let set_fill value dia =
      fun ~fill:_ ~stroke ctx ->
        dia ~fill:value ~stroke ctx

    let overlay dias =
      fun ~fill ~stroke ctx ->
        dias |> List.fold_left (fun () dia -> dia ~fill ~stroke ctx) ()

    let rotate ~radians dia =
      fun ~fill ~stroke ctx ->
        ctx##save;
        ctx##rotate radians;
        dia ~fill ~stroke ctx;
        ctx##restore

    let translate (dx, dy) dia =
      fun ~fill ~stroke ctx ->
        ctx##save;
        ctx##translate dx dy;
        dia ~fill ~stroke ctx;
        ctx##restore

    let run dia ctx =
      dia ~fill:false ~stroke:false ctx

  end

end

let canvas_width = 400.0
let canvas_height = 400.0

let apex_diameter = 3.0
let branch_len = 6.0
let fork_angle = 45.0

let draw_tree (type d) (module D : Diagram.S with type t = d) : Binary_tree.tree -> d =
  let rec draw_tree (tree : Binary_tree.tree) =
    match tree with
    | Apex ->
        D.circle ~diameter:apex_diameter
          |> D.set_fill true

    | Fork (tree1, tree2) ->
        D.overlay [
          D.rotate ~radians:(+.fork_angle *. Float.pi /. 180.0) (draw_tree tree1);
          D.rotate ~radians:(-.fork_angle *. Float.pi /. 180.0) (draw_tree tree2);
        ]

    | Branch tree ->
        D.overlay [
          D.line (0.0, 0.0) (0.0, -.branch_len)
            |> D.set_stroke true;
          D.translate (0.0, -.branch_len) (draw_tree tree);
        ]
  in
  draw_tree

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
      (draw_tree (module Diagram.Canvas) (Binary_tree.grow 5));

    ctx##restore;
  end;

  Js._false
end

let () = begin
  Html.window##.onload := Html.handler start;
end
