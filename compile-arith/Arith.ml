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
    | TreeLang.Neg n -> translate n @ [StackLang.Neg]
    | TreeLang.Add (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Add]
    | TreeLang.Sub (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Sub]
    | TreeLang.Mul (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Mul]
    | TreeLang.Div (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Div]

end
