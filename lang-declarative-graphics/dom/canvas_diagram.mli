(** A declarative version of the Canvas API *)

module Html := Js_of_ocaml.Dom_html
module Js := Js_of_ocaml.Js

type t

include Diagram.S with type t := t

val run : t -> Html.canvasRenderingContext2D Js.t -> unit
