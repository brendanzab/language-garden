(** {0 Example production systems} *)


(** Lindenmayer's original L-system for modelling the growth of algae. *)
module Algae : LSystem.Grammar

(** Development of a multicelluar filament in the blue-green bactieria,
    {{: https://en.wikipedia.org/wiki/Anabaena} Anabaena catenula}. *)
module Filament : LSystem.Grammar

(** Quadratic {{:https://en.wikipedia.org/wiki/Koch_snowflake} Koch island} *)
module KochIsland : LSystem.Grammar

(** A parametric L-System *)
module Parametric : LSystem.Grammar

(** A fractal binary tree *)
module BinaryTree : LSystem.Grammar

(** Cantor’s fractal set *)
module CantorSet : LSystem.Grammar
