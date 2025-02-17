(** A declarative version of the Canvas API *)

module Diagram := Declarative_graphics_core.Diagram
module Html := Js_of_ocaml.Dom_html
module Js := Js_of_ocaml.Js

type t

include Diagram.S with type t := t

(** Render the diagram in the supplied canvas rendering context *)
val run : Html.canvasRenderingContext2D Js.t -> t -> unit
