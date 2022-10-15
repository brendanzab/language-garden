type source = TreeLang.expr
type target = StackLang.code


(** Function composition *)
let (<<) f g x = f (g x)

(** Translate an expression, adding it to the continuation of the expression.
    The continuation allows us to avoid using list concatenation, which could
    lead to exponential blowups during compilation. *)
let rec translate_code : TreeLang.expr -> StackLang.code -> StackLang.code =
  function
  | TreeLang.Var n -> List.cons (StackLang.Access n)
  | TreeLang.Let (_, e1, e2) ->
      translate_code e1
        << List.cons StackLang.BeginLet
        << translate_code e2
        << List.cons StackLang.EndLet
  | TreeLang.Int i -> List.cons (StackLang.Int i)
  | TreeLang.Bool b -> List.cons (StackLang.Bool b)
  | TreeLang.Neg e -> translate_code e << List.cons StackLang.Neg
  | TreeLang.Add (e1, e2) -> translate_code e1 << translate_code e2 << List.cons StackLang.Add
  | TreeLang.Sub (e1, e2) -> translate_code e1 << translate_code e2 << List.cons StackLang.Sub
  | TreeLang.Mul (e1, e2) -> translate_code e1 << translate_code e2 << List.cons StackLang.Mul
  | TreeLang.Div (e1, e2) -> translate_code e1 << translate_code e2 << List.cons StackLang.Div
  | TreeLang.Eq (e1, e2) -> translate_code e1 << translate_code e2 << List.cons StackLang.Eq
  | TreeLang.IfThenElse(e1, e2, e3) ->
      translate_code e1
        << List.cons (StackLang.Code (translate_code e2 []))
        << List.cons (StackLang.Code (translate_code e3 []))
        << List.cons StackLang.IfThenElse


let translate (e : TreeLang.expr) : StackLang.code =
  translate_code e []
