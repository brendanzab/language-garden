type source = Tree_lang.expr
type target = Anf_lang.expr


module Env = struct

  (* Reader + Continuation monad *)

  (* TODO: Add fresh variable state to environment *)

  type env = Anf_lang.atom list

  let empty = []

  type 'a cont = env -> 'a -> Anf_lang.expr


  type 'a t = 'a cont cont

  let pure (type a) (x : a) : a t =
    fun env cont -> cont env x

  let bind (type a b) (x : a t) (f : a -> b t) : b t =
    fun env cont ->
      x env (fun _ x -> f x env cont)


  let embed : type a. a cont cont -> a t = Fun.id
  let run : type a. a t -> a cont cont = Fun.id

  let scope_env (type a) (f : env -> env) (x : a t) : a t=
    fun env cont -> x (f env) cont

  let get_env : env t =
    fun env cont -> cont env env

  let get_var (n : Tree_lang.index) : Anf_lang.atom t =
    fun env cont -> cont env (List.nth env n)


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
    | Tree_lang.Var n ->
        let* e = get_var n in
        pure (Anf_lang.Atom e)
    | Tree_lang.Let (n, e1, e2) ->
        let* e1 = translate_name n e1 in
        scope_env (List.cons e1) (translate e2)
    | Tree_lang.Int i ->
        pure (Anf_lang.Atom (Anf_lang.Int i))
    | Tree_lang.Bool i ->
        pure (Anf_lang.Atom (Anf_lang.Bool i))
    | Tree_lang.Neg e ->
        let* e = translate_name "x" e in
        pure (Anf_lang.Neg e)
    | Tree_lang.Add (e1, e2) ->
        let* e1 = translate_name "x" e1 in
        let* e2 = translate_name "y" e2 in
        pure (Anf_lang.Add (e1, e2))
    | Tree_lang.Sub (e1, e2) ->
        let* e1 = translate_name "x" e1 in
        let* e2 = translate_name "y" e2 in
        pure (Anf_lang.Sub (e1, e2))
    | Tree_lang.Mul (e1, e2) ->
        let* e1 = translate_name "x" e1 in
        let* e2 = translate_name "y" e2 in
        pure (Anf_lang.Mul (e1, e2))
    | Tree_lang.Div (e1, e2) ->
        let* e1 = translate_name "x" e1 in
        let* e2 = translate_name "y" e2 in
        pure (Anf_lang.Div (e1, e2))
    | Tree_lang.Eq (e1, e2) ->
        let* e1 = translate_name "x" e1 in
        let* e2 = translate_name "y" e2 in
        pure (Anf_lang.Eq (e1, e2))
    | Tree_lang.IfThenElse (e1, e2, e3) ->
        let* e1 = translate_name "b" e1 in
        fun env cont ->
          let jx = fresh_id () in
          let px = fresh_id () in
          Anf_lang.LetJoin ("j", jx, ("p", px), cont env (Anf_lang.Atom (Var px)),
            Anf_lang.IfThenElse (e1,
              translate_name "true" e2 env (Fun.const (Anf_lang.join_app jx)),
              translate_name "false" e3 env (Fun.const (Anf_lang.join_app jx))))

  (** Translate an expression to ANF, binding the resulting computation to an
      intermediate definition (so long as it’s not an atomic computation). *)
  and translate_name (n : string) (e : Tree_lang.expr) : Anf_lang.atom t =
    let* e = translate e in
    match e with
    (* Don't bother binding definitions for atomic computations *)
    | Anf_lang.Atom a -> pure a
    (* Bind definitions for non-atomic computations *)
    | e -> fun env cont ->
        let x = fresh_id () in
        Anf_lang.Let (n, x, e, cont env (Anf_lang.Var x))

end


let translate (e : Tree_lang.expr) : Anf_lang.expr =
  Env.run (Env.translate e) Env.empty (Fun.const Anf_lang.comp)
