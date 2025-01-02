module Diagram = Declarative_graphics_core.Diagram
module Dom = Js_of_ocaml.Dom
module Svg = Js_of_ocaml.Dom_svg
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

  type t = state -> Svg.document Js.t -> Svg.element Js.t -> unit

  let empty : t =
    fun _ _ _ -> ()

  let over dia1 dia2 : t =
    fun state doc parent -> begin
      dia2 state doc parent;
      dia1 state doc parent;
    end

  let circle ~diameter : t =
    fun state doc parent ->
      let elem = Svg.createCircle doc in
      elem##setAttribute (Js.string "r") (Js.string (string_of_float (diameter *. 0.5)));
      (match state.fill_style with
        | `solid -> elem##setAttribute (Js.string "fill") (Js.string "black")
        | `none -> ());
      (match state.stroke_style with
        | `solid -> elem##setAttribute (Js.string "stroke") (Js.string "black")
        | `none -> ());
      Dom.appendChild parent elem

  let line (x1, y1) (x2, y2) : t =
    fun state doc parent ->
      let elem = Svg.createLineElement doc in
      elem##setAttribute (Js.string "x1") (Js.string (string_of_float x1));
      elem##setAttribute (Js.string "y1") (Js.string (string_of_float y1));
      elem##setAttribute (Js.string "x2") (Js.string (string_of_float x2));
      elem##setAttribute (Js.string "y2") (Js.string (string_of_float y2));
      (match state.stroke_style with
        | `solid -> elem##setAttribute (Js.string "stroke") (Js.string "black")
        | `none -> ());
      Dom.appendChild parent elem

  let stroke style dia : t =
    fun state ctx ->
      dia { state with stroke_style = style } ctx

  let fill style dia =
    fun state ctx ->
      dia { state with fill_style = style } ctx

  let rotate ~radians dia : t =
    fun state doc parent ->
      let elem = Svg.createG doc in
      elem##setAttribute (Js.string "transform") (Js.string ("rotate(" ^ string_of_float (radians *. 180.0 /. Float.pi) ^ ")"));
      dia state doc (elem :> Svg.element Js.t);
      Dom.appendChild parent elem

  let translate (dx, dy) dia : t =
    fun state doc parent ->
      let elem = Svg.createG doc in
      elem##setAttribute (Js.string "transform") (Js.string ("translate(" ^ string_of_float dx ^ ", " ^ string_of_float dy ^ ")"));
      dia state doc (elem :> Svg.element Js.t);
      Dom.appendChild parent elem

  let scale xy dia : t =
    fun state doc parent ->
      let elem = Svg.createG doc in
      elem##setAttribute (Js.string "transform") (Js.string ("scale(" ^ string_of_float xy ^ ")"));
      dia state doc (elem :> Svg.element Js.t);
      Dom.appendChild parent elem

end

include Diagram.Make (Core)

let run (dia : t) (doc : Svg.document Js.t) (* attributes *) : Svg.svgElement Js.t =
  let elem = Svg.createSvg doc in
  dia default_state doc (elem :> Svg.element Js.t);
  elem
