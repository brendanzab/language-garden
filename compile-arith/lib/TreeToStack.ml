type source = TreeLang.expr
type target = StackLang.code

let rec translate : TreeLang.expr -> StackLang.code =
  function
  | TreeLang.Int i -> [StackLang.Int i]
  | TreeLang.Neg e -> translate e @ [StackLang.Neg]
  | TreeLang.Add (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Add]
  | TreeLang.Sub (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Sub]
  | TreeLang.Mul (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Mul]
  | TreeLang.Div (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Div]
