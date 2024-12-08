(** {0 Intermediate languages} *)

(** {1 Languages using nameless variables} *)

(** Simply typed lambda calculus *)
module Fun = struct

  include Lang_Fun

  module Lexer = Lang_Fun_Lexer
  module Parser = Lang_Fun_Parser

end

(** Closure converted language *)
module Clos = Lang_Clos


(** {1 Alpha-renamed languages} *)

(** Simply typed lambda calculus (alpha-renamed) *)
module FunA = Lang_FunA

(** Closure converted language (alpha-renamed) *)
module ClosA = Lang_ClosA

(** Lambda lifted language (alpha-renamed) *)
module LiftedA = Lang_LiftedA
