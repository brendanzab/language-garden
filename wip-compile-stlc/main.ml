module Surface_lexer = Surface_lexer
module Surface_parser = Surface_parser
module Surface = Surface

let _compile_anf (expr : Surface.expr) : Anf.expr =
  let expr, _ty = Surface.elab_infer [] expr in
  expr
  |> Core_to_anf.translate
  (* TODO: Add more passes here *)

let _compile_monadic (expr : Surface.expr) : Monadic.expr =
  let expr, _ty = Surface.elab_infer [] expr in
  expr
  |> Core_to_monadic.translate
  (* TODO: Add more passes here *)
