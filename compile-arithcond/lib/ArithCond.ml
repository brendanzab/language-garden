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

  let rec translate (e : TreeLang.expr) : AnfLang.expr =
    let (let*) = (@@) in

    let rec go e cont : AnfLang.expr =
      match e with
      | TreeLang.Int i -> cont (AnfLang.Int i)
      | TreeLang.Bool b -> cont (AnfLang.Bool b)
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
      | TreeLang.Eq (e1, e2) ->
          let* e1 = go e1 in
          let* e2 = go e2 in
          make_let (AnfLang.Eq (e1, e2)) cont
      | TreeLang.IfThenElse (e1, e2, e3) ->
          let* a = go e1 in
          (* TODO: Join points? *)
          make_let (AnfLang.IfThenElse (a, translate e2, translate e3)) cont
    in

    go e (fun a -> AnfLang.Atom a)

end
