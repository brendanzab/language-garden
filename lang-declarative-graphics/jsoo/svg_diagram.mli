module Diagram := Declarative_graphics_core.Diagram
module Svg := Js_of_ocaml.Dom_svg
module Js := Js_of_ocaml.Js

type t

include Diagram.S with type t := t

val run : t -> Svg.document Js.t -> Svg.svgElement Js.t