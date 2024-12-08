type source = Tree_lang.expr
type target = Anf_lang.expr


module Env = struct

  (* Continuation monad *)

  (* TODO: Add fresh variable state to environment *)

  type 'a cont = 'a -> Anf_lang.expr

  type 'a t = 'a cont cont

  let pure (type a) (x : a) : a t =
    fun cont -> cont x

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun cont -> x (fun x -> f x cont)

  let embed : type a. a cont cont -> a t = Fun.id
  let run : type a. a t -> a cont cont = Fun.id


  (* ANF Translation *)

  type source = Tree_lang.expr
  type target = Anf_lang.comp t

  (* TODO: Move fresh variable generation into monad *)

  (** Generate a fresh variable id *)
  let fresh_id : unit -> Anf_lang.id =
    let next_id = ref 0 in
    fun () ->
      let n = !next_id in
      incr next_id;
      n

  let ( let* ) = bind

  let rec translate (e : Tree_lang.expr) : Anf_lang.comp t =
    match e with
    | Tree_lang.Int i ->
        pure (Anf_lang.Atom (Anf_lang.Int i))
    | Tree_lang.Neg e ->
        let* e = translate_name e in
        pure (Anf_lang.Neg e)
    | Tree_lang.Add (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (Anf_lang.Add (e1, e2))
    | Tree_lang.Sub (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (Anf_lang.Sub (e1, e2))
    | Tree_lang.Mul (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (Anf_lang.Mul (e1, e2))
    | Tree_lang.Div (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (Anf_lang.Div (e1, e2))

  (** Translate an expression to ANF, binding the resulting computation to an
      intermediate definition (so long as it’s not an atomic computation). *)
  and translate_name (e : Tree_lang.expr) : Anf_lang.atom t =
    let* e = translate e in
    match e with
    (* Don't bother binding definitions for atomic computations *)
    | Anf_lang.Atom a -> pure a
    (* Bind definitions for non-atomic computations *)
    | e -> fun cont ->
        let n = fresh_id () in
        Anf_lang.Let (n, e, cont (Anf_lang.Var n))

end


let translate (e : Tree_lang.expr) : Anf_lang.expr =
  Env.run (Env.translate e) Anf_lang.comp
