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
    | TreeLang.Int i -> [StackLang.Int i]
    | TreeLang.Bool b -> [StackLang.Bool b]
    | TreeLang.Neg e -> translate e @ [StackLang.Neg]
    | TreeLang.Add (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Add]
    | TreeLang.Sub (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Sub]
    | TreeLang.Mul (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Mul]
    | TreeLang.Div (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Div]
    | TreeLang.Eq (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Eq]
    | TreeLang.IfThenElse(e1, e2, e3) -> translate e1
        @ [StackLang.Code (translate e2); StackLang.Code (translate e3); StackLang.IfThenElse]

end
