type source = TreeLang.expr
type target = StackLang.code

let rec translate : TreeLang.expr -> StackLang.code =
  function
  | TreeLang.Var n -> [StackLang.Access n]
  | TreeLang.Let (_, e1, e2) ->
      translate e1 @ [StackLang.BeginLet] @ translate e2 @ [StackLang.EndLet]
  | TreeLang.Int i -> [StackLang.Int i]
  | TreeLang.Bool b -> [StackLang.Bool b]
  | TreeLang.Neg e -> translate e @ [StackLang.Neg]
  | TreeLang.Add (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Add]
  | TreeLang.Sub (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Sub]
  | TreeLang.Mul (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Mul]
  | TreeLang.Div (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Div]
  | TreeLang.Eq (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Eq]
  | TreeLang.IfThenElse(e1, e2, e3) ->
      translate e1 @ [StackLang.Code (translate e2); StackLang.Code (translate e3); StackLang.IfThenElse]
