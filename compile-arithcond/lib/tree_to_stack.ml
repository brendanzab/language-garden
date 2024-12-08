type source = Tree_lang.expr
type target = Stack_lang.code


(** Function composition *)
let ( << ) f g x = f (g x)

(** Translate an expression, adding it to the continuation of the expression.
    The continuation allows us to avoid using list concatenation, which could
    lead to exponential blowups during compilation. *)
let rec translate_code : Tree_lang.expr -> Stack_lang.code -> Stack_lang.code =
  function
  | Tree_lang.Var n -> List.cons (Stack_lang.Access n)
  | Tree_lang.Let (_, e1, e2) ->
      translate_code e1
        << List.cons Stack_lang.BeginLet
        << translate_code e2
        << List.cons Stack_lang.EndLet
  | Tree_lang.Int i -> List.cons (Stack_lang.Int i)
  | Tree_lang.Bool b -> List.cons (Stack_lang.Bool b)
  | Tree_lang.Neg e -> translate_code e << List.cons Stack_lang.Neg
  | Tree_lang.Add (e1, e2) -> translate_code e1 << translate_code e2 << List.cons Stack_lang.Add
  | Tree_lang.Sub (e1, e2) -> translate_code e1 << translate_code e2 << List.cons Stack_lang.Sub
  | Tree_lang.Mul (e1, e2) -> translate_code e1 << translate_code e2 << List.cons Stack_lang.Mul
  | Tree_lang.Div (e1, e2) -> translate_code e1 << translate_code e2 << List.cons Stack_lang.Div
  | Tree_lang.Eq (e1, e2) -> translate_code e1 << translate_code e2 << List.cons Stack_lang.Eq
  | Tree_lang.IfThenElse(e1, e2, e3) ->
      translate_code e1
        << List.cons (Stack_lang.Code (translate_code e2 []))
        << List.cons (Stack_lang.Code (translate_code e3 []))
        << List.cons Stack_lang.IfThenElse


let translate (e : Tree_lang.expr) : Stack_lang.code =
  translate_code e []
