let _compile_anf (expr : Core.expr) : Anf.expr =
  expr
  |> CoreToAnf.translate
  (* TODO: Add more passes here *)

let _compile_monadic (expr : Core.expr) : Monadic.expr =
  expr
  |> CoreToMonadic.translate
  (* TODO: Add more passes here *)
