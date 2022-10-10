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
  type target = AnfLang.comp t

  (** Generate a fresh variable id *)
  let fresh_id : unit -> AnfLang.id =
    (* TODO: Add this to the monad *)
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      id

  let (let*) = bind

  let rec translate (e : TreeLang.expr) : AnfLang.comp t =
    match e with
    | TreeLang.Int i ->
        pure (AnfLang.Atom (AnfLang.Int i))
    | TreeLang.Neg e ->
        let* e = translate_name e in
        pure (AnfLang.Neg e)
    | TreeLang.Add (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (AnfLang.Add (e1, e2))
    | TreeLang.Sub (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (AnfLang.Sub (e1, e2))
    | TreeLang.Mul (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (AnfLang.Mul (e1, e2))
    | TreeLang.Div (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (AnfLang.Div (e1, e2))

  and translate_name (e : TreeLang.expr) : AnfLang.atom t =
    let* e = translate e in
    match e with
    | AnfLang.Atom a -> pure a
    | e ->
        let n = fresh_id () in
        fun cont ->
          AnfLang.Let (n, e, cont (AnfLang.Var n))

end


let translate (e : TreeLang.expr) : AnfLang.expr =
  Env.run (Env.translate e) (fun c -> AnfLang.Comp c)
