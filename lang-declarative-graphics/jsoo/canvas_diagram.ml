module Diagram = Declarative_graphics_core.Diagram
module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

type state = {
  fill_style : Diagram.style;
  stroke_style : Diagram.style;
}

let default_state = {
  fill_style =`none;
  stroke_style = `none;
}

module Core = struct

  type t = state -> Html.canvasRenderingContext2D Js.t -> unit

  let empty =
    fun _ _ -> ()

  let over dia1 dia2 =
    fun state ctx -> begin
      dia2 state ctx;
      dia1 state ctx;
    end

  let arc ~center:(center_x, center_y) ~radius ~theta1 ~theta2 ~ccw =
    fun state ctx ->
      ctx##beginPath;
      ctx##arc
        (Js.float center_x)
        (Js.float center_y)
        (Js.float radius)
        (Js.float theta1)
        (Js.float theta2)
        (Js.bool ccw);
      (match state.fill_style with `solid -> ctx##fill | `none -> ());
      (match state.stroke_style with `solid -> ctx##stroke | `none -> ());
      ()

  let circle ~diameter =
    arc
      ~center:(0.0, 0.0)
      ~radius:(diameter *. 0.5)
      ~theta1:0.0
      ~theta2:(2.0 *. Float.pi)
      ~ccw:false

  let line (x1, y1) (x2, y2) =
    fun state ctx ->
      match state.stroke_style with
      | `solid ->
          ctx##beginPath;
          ctx##moveTo x1 y1;
          ctx##lineTo x2 y2;
          ctx##stroke;
      | `none -> ()

  let stroke style dia =
    fun state ctx ->
      dia { state with stroke_style = style } ctx

  let fill style dia =
    fun state ctx ->
      dia { state with fill_style = style } ctx

  let rotate ~radians dia =
    fun state ctx ->
      ctx##save;
      ctx##rotate radians;
      dia state ctx;
      ctx##restore

  let translate (dx, dy) dia =
    fun state ctx ->
      ctx##save;
      ctx##translate dx dy;
      dia state ctx;
      ctx##restore

  let scale xy dia =
    fun state ctx ->
      ctx##save;
      ctx##scale xy xy;
      dia state ctx;
      ctx##restore

end

include Diagram.Make (Core)

let run dia ctx =
  dia default_state ctx
