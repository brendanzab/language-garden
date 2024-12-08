(** Declarative diagramming API *)

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

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

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
