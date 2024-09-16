(** An interpreter for the lambda calculus, implemented using the CEK machine.

    The CEK machine is an approach for modelling the call-by-value lambda
    calculus in terms of an abstract machine. The name “CEK” is derived from the
    components of the machine state:

    ⟨ C, E, K ⟩
      ▲  ▲  ▲
      │  │  │
      │  │  The (K)ontinuation
      │  │
      │  The (E)nvironment
      │
      The (C)ontrol instruction

    By structuring evaluation in this way, it means that:

    - evaluation can be implemented tail-recursively
    - the interpreter can be stepped, which could be useful for debugging

    The CESK machine extends the CEK machine with a store/heap. This is used to
    model mutable state.

    {2 Resources}

    - {{:https://en.wikipedia.org/wiki/CEK_Machine} CEK Machine} on Wikipedia
    - {{:https://matt.might.net/articles/cek-machines/} Writing CEK-style interpreters (or semantics) in Haskell} by Matt Might
    - {{:https://github.com/kmicinski/cmsc330examples/blob/master/lambda-calculus/cek-lambda.ml} cek-lambda.ml} by Kristopher Micinski
    - {{:https://doi.org/10.1145/173262.155113} The essence of compiling with continuations} by Felleisen and Friedman
*)

(** {1 Syntax} *)

type expr =
  | Var of int                      (* variables, represented with de Bruijn indices *)
  | Let of string * expr * expr
  | FunLit of string * expr
  | FunApp of expr * expr


(** {1 Semantics} *)

type value =
  | FunLit of clos

and clos =
  | Clos of env * expr

and env = value list


(** Defunctionalised continuation *)
type cont =
  | Done
  (** The empty continuation *)

  | LetBody of (string * unit * expr) * env * cont
  (** Continue by evaluating the body of a let binding *)

  | FunArg of (unit * expr) * env * cont
  (** Continue a function application by evaluting the argument *)

  | FunApp of (value * unit) * cont
  (** Continue by applying a function to an argument *)


(** The state of the abstract machine *)
type state =
  | Eval of expr * env * cont
  (** Evaluate an expression, where:

      - [expr] is the control instruction
      - [env] is the environment
      - [cont] is the continuation
  *)

  | Cont of cont * value
  (** Apply a continuation to a value *)


(** Move the execution of the abstract machine forwards by one step. *)
let step (s : state) : state =
  match s with
  (* Evaluating a variable? Lookup the variable in the environment and apply it
     to the continuation. *)
  | Eval (Var x, env, k) ->
      Cont (k, List.nth env x)

  (* Evaluating a let expression? Evaluate the definition, leaving the
     evaluation of body until later. *)
  | Eval (Let (x, def, body), env, k) ->
      Eval (def, env, LetBody ((x, (), body), env, k))

  (* Evaluating a function literal? Convert it to a value and apply it to the
     continuation. *)
  | Eval (FunLit (x, body), env, k) ->
      Cont (k, FunLit (Clos (env, body)))

  (* Evaluating a function application? Evaluate the head of the application,
     leaving the evaluation of the argument until later. *)
  | Eval (FunApp (head, arg), env, k) ->
      Eval (head, env, FunArg (((), arg), env, k))


  (* Add the definition to the environment and evaluate the body. *)
  | Cont (LetBody ((_, (), body), env, k), def) ->
      Eval (body, def :: env, k)

  (* Evaluate an argument, leaving the final function application until later. *)
  | Cont (FunArg (((), arg), env, k), head) ->
      Eval (arg, env, FunApp ((head, ()), k))

  (* Add the argument to the environment of the closure and evaluate the body. *)
  | Cont (FunApp ((FunLit (Clos (env, body)), ()), k), arg) ->
      Eval (body, arg :: env, k)


  (* Attempted to continue the empty continuation *)
  | Cont (Done, _) ->
      failwith "cannot step evaluation further"


(** Compute the value of an expression tail-recusively *)
let eval (e : expr) : value =
  (* Step the machine state until we reach the empty continuation *)
  let rec go (s : state) : value =
    match s with
    | Cont (Done, v) -> v
    | s -> (go [@tailcall]) (step s)
  in
  go (Eval (e, [], Done))
