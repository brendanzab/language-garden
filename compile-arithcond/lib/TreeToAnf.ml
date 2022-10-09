type source = TreeLang.expr
type target = AnfLang.expr


module Env = struct

  (* Continuation monad *)

  (* TODO: Add fresh variable state to environment *)

  type 'a cont = 'a -> AnfLang.expr

  type 'a t = 'a cont cont

  let pure x = fun cont -> cont x
  let bind x f = fun cont -> x (fun x -> f x cont)

  let embed = fun cont -> cont
  let run x = fun cont -> x cont


  (* ANF Translation *)

  type source = TreeLang.expr
  type target = AnfLang.atom t

  (** Generate a fresh variable id *)
  let fresh_id : unit -> AnfLang.id =
    (* TODO: Add this to the monad *)
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      id

  (** Bind the result of a computation to a shared definition. *)
  let let_comp (c : AnfLang.comp) : AnfLang.atom t =
    fun cont ->
      let id = fresh_id () in
      AnfLang.Let (id, c, cont (AnfLang.Var id))

  let (let*) = bind

  let rec translate (e : TreeLang.expr) : AnfLang.atom t =
    match e with
    | TreeLang.Int i ->
        pure (AnfLang.Int i)
    | TreeLang.Bool i ->
        pure (AnfLang.Bool i)
    | TreeLang.Neg e ->
        let* e = translate e in
        let_comp (AnfLang.Neg e)
    | TreeLang.Add (e1, e2) ->
        let* e1 = translate e1 in
        let* e2 = translate e2 in
        let_comp (AnfLang.Add (e1, e2))
    | TreeLang.Sub (e1, e2) ->
        let* e1 = translate e1 in
        let* e2 = translate e2 in
        let_comp (AnfLang.Sub (e1, e2))
    | TreeLang.Mul (e1, e2) ->
        let* e1 = translate e1 in
        let* e2 = translate e2 in
        let_comp (AnfLang.Mul (e1, e2))
    | TreeLang.Div (e1, e2) ->
        let* e1 = translate e1 in
        let* e2 = translate e2 in
        let_comp (AnfLang.Div (e1, e2))
    | TreeLang.Eq (e1, e2) ->
        let* e1 = translate e1 in
        let* e2 = translate e2 in
        let_comp (AnfLang.Eq (e1, e2))
    | TreeLang.IfThenElse (e1, e2, e3) ->
        let* a = translate e1 in
        (* TODO: Join points? *)
        let e2 = translate e2 (fun a -> AnfLang.Atom a) in
        let e3 = translate e3 (fun a -> AnfLang.Atom a) in
        let_comp (AnfLang.IfThenElse (a, e2, e3))

end


let translate (e : TreeLang.expr) : AnfLang.expr =
  Env.run (Env.translate e) (fun a -> AnfLang.Atom a)
