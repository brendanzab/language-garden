(** {0 A compiler for arithmetic expressions} *)

(** {1 Intermediate languages} *)

(** Nested arithmetic expressions *)
module TreeLang = struct
  include TreeLang

  module Lexer = TreeLangLexer
  module Parser = TreeLangParser
end

(** Stack machine language *)
module StackLang = StackLang

(** A-Normal form *)
module AnfLang = AnfLang


(** {1 Compilation} *)

module Translation = Translation

module TreeToStack = TreeToStack
module TreeToAnf = TreeToAnf
