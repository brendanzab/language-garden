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
    | TreeLang.Bool b -> [StackLang.Bool b]
    | TreeLang.Neg t -> translate t @ [StackLang.Neg]
    | TreeLang.Add (t1, t2) -> translate t1 @ translate t2 @ [StackLang.Add]
    | TreeLang.Sub (t1, t2) -> translate t1 @ translate t2 @ [StackLang.Sub]
    | TreeLang.Mul (t1, t2) -> translate t1 @ translate t2 @ [StackLang.Mul]
    | TreeLang.Div (t1, t2) -> translate t1 @ translate t2 @ [StackLang.Div]
    | TreeLang.Eq (t1, t2) -> translate t1 @ translate t2 @ [StackLang.Eq]
    | TreeLang.IfThenElse(t1, t2, t3) -> translate t1
        @ [StackLang.Code (translate t2); StackLang.Code (translate t3); StackLang.IfThenElse]

end
