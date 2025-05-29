(** {0 Intermediate languages} *)

(** {1 Languages using nameless variables} *)

(** Simply typed lambda calculus *)
module Fun = struct

  include Lang__fun

  module Lexer = Lang__fun__lexer
  module Parser = Lang__fun__parser

end

(** Closure converted language *)
module Clos = Lang__clos


(** {1 Alpha-renamed languages} *)

(** Simply typed lambda calculus (alpha-renamed) *)
module Fun_a = Lang__fun_a

(** Closure converted language (alpha-renamed) *)
module Clos_a = Lang__clos_a

(** Lambda lifted language (alpha-renamed) *)
module Lifted_a = Lang__lifted_a
