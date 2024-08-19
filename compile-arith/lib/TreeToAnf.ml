type source = TreeLang.expr
type target = AnfLang.expr


module Env = struct

  (* Continuation monad *)

  (* TODO: Add fresh variable state to environment *)

  type 'a cont = 'a -> AnfLang.expr

  type 'a t = 'a cont cont

  let pure (type a) (x : a) : a t =
    fun cont -> cont x

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun cont -> x (fun x -> f x cont)

  let embed : type a. a cont cont -> a t = Fun.id
  let run : type a. a t -> a cont cont = Fun.id


  (* ANF Translation *)

  type source = TreeLang.expr
  type target = AnfLang.comp t

  (* TODO: Move fresh variable generation into monad *)

  (** Generate a fresh variable id *)
  let fresh_id : unit -> AnfLang.id =
    let next_id = ref 0 in
    fun () ->
      let n = !next_id in
      incr next_id;
      n

  let ( let* ) = bind

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

  (** Translate an expression to ANF, binding the resulting computation to an
      intermediate definition (so long as it’s not an atomic computation). *)
  and translate_name (e : TreeLang.expr) : AnfLang.atom t =
    let* e = translate e in
    match e with
    (* Don't bother binding definitions for atomic computations *)
    | AnfLang.Atom a -> pure a
    (* Bind definitions for non-atomic computations *)
    | e -> fun cont ->
        let n = fresh_id () in
        AnfLang.Let (n, e, cont (AnfLang.Var n))

end


let translate (e : TreeLang.expr) : AnfLang.expr =
  Env.run (Env.translate e) AnfLang.comp
