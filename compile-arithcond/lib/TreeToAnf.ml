type source = TreeLang.expr
type target = AnfLang.expr


module Env = struct

  (* Reader + Continuation monad *)

  (* TODO: Add fresh variable state to environment *)

  type env = AnfLang.atom list

  let empty = []

  type 'a cont = env -> 'a -> AnfLang.expr


  type 'a t = 'a cont cont

  let pure (x : 'a) : 'a t =
    fun env cont -> cont env x

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    fun env cont ->
      x env (fun _ x -> f x env cont)


  let embed : 'a cont cont -> 'a t = Fun.id
  let run : 'a t -> 'a cont cont = Fun.id

  let scope_env (f : env -> env) (x : 'a t) : 'a t=
    fun env cont -> x (f env) cont

  let get_env : env t =
    fun env cont -> cont env env

  let get_var (n : TreeLang.index) : AnfLang.atom t =
    fun env cont -> cont env (List.nth env n)


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

  let (let*) = bind

  let rec translate (e : TreeLang.expr) : AnfLang.comp t =
    match e with
    | TreeLang.Var n ->
        let* e = get_var n in
        pure (AnfLang.Atom e)
    | TreeLang.Let (_, e1, e2) ->
        let* e1 = translate_name e1 in
        scope_env (fun env -> e1 :: env) (translate e2)
    | TreeLang.Int i ->
        pure (AnfLang.Atom (AnfLang.Int i))
    | TreeLang.Bool i ->
        pure (AnfLang.Atom (AnfLang.Bool i))
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
    | TreeLang.Eq (e1, e2) ->
        let* e1 = translate_name e1 in
        let* e2 = translate_name e2 in
        pure (AnfLang.Eq (e1, e2))
    | TreeLang.IfThenElse (e1, e2, e3) ->
        let* e1 = translate_name e1 in
        let* env = get_env in
        (* TODO: Join points? *)
        let e2 = translate e2 env (Fun.const AnfLang.comp) in
        let e3 = translate e3 env (Fun.const AnfLang.comp) in
        pure (AnfLang.IfThenElse (e1, e2, e3))

  (** Translate an expression to ANF, binding the resulting computation to an
      intermediate definition (so long as it’s not an atomic computation). *)
  and translate_name (e : TreeLang.expr) : AnfLang.atom t =
    let* e = translate e in
    match e with
    (* Don't bother binding definitions for atomic computations *)
    | AnfLang.Atom a -> pure a
    (* Bind definitions for non-atomic computations *)
    | e -> fun env cont ->
        let n = fresh_id () in
        AnfLang.Let (n, e, cont env (AnfLang.Var n))

end


let translate (e : TreeLang.expr) : AnfLang.expr =
  Env.run (Env.translate e) Env.empty (Fun.const AnfLang.comp)
