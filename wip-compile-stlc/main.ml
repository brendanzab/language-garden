module SurfaceLexer = SurfaceLexer
module SurfaceParser = SurfaceParser
module Surface = Surface

let _compile_anf (expr : Surface.expr) : Anf.expr =
  let expr, _ty = Surface.elab_infer [] expr in
  expr
  |> CoreToAnf.translate
  (* TODO: Add more passes here *)

let _compile_monadic (expr : Surface.expr) : Monadic.expr =
  let expr, _ty = Surface.elab_infer [] expr in
  expr
  |> CoreToMonadic.translate
  (* TODO: Add more passes here *)
