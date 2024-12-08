(** Declarative diagramming API *)

type style = [`solid | `none]
type vec2 = float * float

module type S = sig

  type t

  val circle : diameter:float -> t
  val line : vec2 -> vec2 -> t

  val stroke : style -> t -> t
  val fill : style -> t -> t

  val stack : t list -> t
  val rotate : radians:float -> t -> t
  val translate : vec2 -> t -> t

end

(** A declarative version of the Canvas API *)
module Canvas : sig

  module Html := Js_of_ocaml.Dom_html
  module Js := Js_of_ocaml.Js

  type t

  include S with type t := t

  val run : t -> Html.canvasRenderingContext2D Js.t -> unit

end = struct

  module Html = Js_of_ocaml.Dom_html
  module Js = Js_of_ocaml.Js

  type state = {
    fill_style : style;
    stroke_style : style;
  }

  let default_style = {
    fill_style =`none;
    stroke_style = `none;
  }

  type t = state -> Html.canvasRenderingContext2D Js.t -> unit

  let arc ~center:(center_x, center_y) ~radius ~theta1 ~theta2 ~ccw =
    fun state ctx ->
      ctx##arc
        (* center_x *) (Js.float center_x)
        (* center_y *) (Js.float center_y)
        (* radius *) (Js.float radius)
        (* theta1 *) (Js.float theta1)
        (* theta2 *) (Js.float theta2)
        (* ccw *) (Js.bool ccw);
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

  let stack dias =
    fun state ctx ->
      dias |> List.fold_left (fun () dia -> dia state ctx) ()

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

  let run dia ctx =
    dia default_style ctx

end
