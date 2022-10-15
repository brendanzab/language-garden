(** {0 Example production systems} *)


(** Lindenmayer's original L-system for modelling the growth of algae. *)
module Algae : LSystem.S

(** Development of a multicelluar filament in the blue-green bactieria,
    {{: https://en.wikipedia.org/wiki/Anabaena} Anabaena catenula}. *)
module Filament : LSystem.S
