(** {0 A compiler for arithmetic expressions} *)

(** {1 Intermediate languages} *)

(** Nested arithmetic expressions *)
module Tree_lang = struct
  include Tree_lang

  module Lexer = Tree_lang_lexer
  module Parser = Tree_lang_parser
end

(** Stack machine language *)
module Stack_lang = Stack_lang

(** A-Normal form *)
module Anf_lang = Anf_lang


(** {1 Compilation} *)

module Translation = Translation

module Tree_to_stack = Tree_to_stack
module Tree_to_anf = Tree_to_anf
