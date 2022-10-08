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
