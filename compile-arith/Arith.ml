module TreeLang = struct
  include TreeLang

  module Lexer = TreeLangLexer
  module Parser = TreeLangParser
end

module StackLang = StackLang


(** A translation pass between two languages *)
module type Translation = sig

  (** The source language *)
  type source

  (** The target language *)
  type target

  (** The translation pass between the {!source} and {!target} languages *)
  val translate : source -> target

end


(** Translation pass between the {!TreeLang} and {!StackLang} *)
module TreeToStack : Translation

  with type source = TreeLang.expr
  with type target = StackLang.code

= struct

  type source = TreeLang.expr
  type target = StackLang.code

  let rec translate : TreeLang.expr -> StackLang.code =
    function
    | TreeLang.Num n -> [StackLang.Num n]
    | TreeLang.Neg e -> translate e @ [StackLang.Neg]
    | TreeLang.Add (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Add]
    | TreeLang.Sub (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Sub]
    | TreeLang.Mul (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Mul]
    | TreeLang.Div (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Div]

end
