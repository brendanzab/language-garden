(** From section 3.5 of {{:http://algorithmicbotany.org/papers/cpfg.agtive99.html}
    “An L-System-Based Plant Modeling Language”} *)

(** The development of an individual flower *)
module Flower = struct

  (** The state of a flower *)
  type symbol =
    | Bud
    | Blossom
    | Fruit

  let axiom =
    [Bud; Blossom; Fruit]

  let rules =
    function
    | Bud -> [Blossom]
    | Blossom -> [Fruit]
    (* TODO: Terminal symbols *)
    | s -> [s]


  let string_of_symbol =
    function
    | Bud -> "A"
    | Blossom -> "B"
    | Fruit -> "C"

  let string_of_word w =
    List.map string_of_symbol w
      |> String.concat ""

  (* TODO: Graphical interpretation? *)

end


(** The state of a stem *)
type symbol =
  | Apex                      (** The apex, or terminal bud *)
  | Stem                      (** An internode/stem in the inflorence *)
  (* TODO: Sub-L-systems *)
  | Flower of Flower.symbol

let symbol_of_flower s =
  Flower s


let axiom =
  [Apex]

let rules =
  function
  | Apex -> [Stem; Flower Bud; Apex]
  (* TODO: Sub-L-systems *)
  | Flower s -> List.map symbol_of_flower (Flower.rules s)
  (* TODO: Terminal symbols *)
  | s -> [s]


let string_of_symbol =
  function
  | Apex -> "A"
  | Stem -> "I"
  | Flower s -> String.concat "" ["["; Flower.string_of_symbol s; "]"]

let string_of_word w =
  List.map string_of_symbol w
    |> String.concat ""

(* TODO: Graphical interpretation? *)
