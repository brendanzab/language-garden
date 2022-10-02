module TreeLang = struct
  include TreeLang

  module Lexer = TreeLangLexer
  module Parser = TreeLangParser
end

module StackLang = StackLang
module AnfLang = AnfLang


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
    | TreeLang.Neg e -> translate e @ [StackLang.Neg]
    | TreeLang.Add (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Add]
    | TreeLang.Sub (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Sub]
    | TreeLang.Mul (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Mul]
    | TreeLang.Div (e1, e2) -> translate e1 @ translate e2 @ [StackLang.Div]

end


(** Translation pass between the {!TreeLang} and {!AnfLang} *)
module TreeToAnf : Translation

  with type source = TreeLang.expr
  with type target = AnfLang.expr

= struct

  type source = TreeLang.expr
  type target = AnfLang.expr

  let fresh_id : unit -> AnfLang.id =
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      id

  let make_let c cont : AnfLang.expr =
    let id = fresh_id () in
    AnfLang.Let (id, c, cont (AnfLang.Var id))

  let translate (e : TreeLang.expr) : AnfLang.expr =
    let (let*) = (@@) in

    let rec go e cont : AnfLang.expr =
      match e with
      | TreeLang.Int i -> cont (AnfLang.Int i)
      | TreeLang.Neg e ->
          let* e = go e in
          make_let (AnfLang.Neg e) cont
      | TreeLang.Add (e1, e2) ->
          let* e1 = go e1 in
          let* e2 = go e2 in
          make_let (AnfLang.Add (e1, e2)) cont
      | TreeLang.Sub (e1, e2) ->
          let* e1 = go e1 in
          let* e2 = go e2 in
          make_let (AnfLang.Sub (e1, e2)) cont
      | TreeLang.Mul (e1, e2) ->
          let* e1 = go e1 in
          let* e2 = go e2 in
          make_let (AnfLang.Mul (e1, e2)) cont
      | TreeLang.Div (e1, e2) ->
          let* e1 = go e1 in
          let* e2 = go e2 in
          make_let (AnfLang.Div (e1, e2)) cont
    in

    go e (fun a -> AnfLang.Atom a)

end
