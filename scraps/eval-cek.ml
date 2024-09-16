(** An interpreter for the lambda calculus, implemented using the CEK machine.

    The CEK machine is an approach for modelling the call-by-value lambda
    calculus in terms of an abstract machine. The name “CEK” is derived from the
    components of the machine state:

    ⟨ C, E, K ⟩
      ▲  ▲  ▲
      │  │  │
      │  │  The continuation
      │  │
      │  The environment
      │
      The control instruction

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
  | Var of string
  | Let of string * expr * expr
  | FunLit of string * expr
  | FunApp of expr * expr


(** {1 Semantics} *)

type value =
  | FunLit of clos

and clos =
  | Clos of env * string * expr

and env = (string * value) list


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
  (* Evaluating a variable? Look it up in the environment and apply it to the continuation. *)
  | Eval (Var x, env, k) ->
      Cont (k, List.assoc x env)

  (* Evaluating a let expression? First evaluate the definition. *)
  | Eval (Let (x, def, body), env, k) ->
      Eval (def, env, LetBody ((x, (), body), env, k))

  (* Evaluating a function? Convert it to a value and apply it to the continuation. *)
  | Eval (FunLit (x, body), env, k) ->
      Cont (k, FunLit (Clos (env, x, body)))

  (* Evaluating a function application? First evaluate the function. *)
  | Eval (FunApp (head, arg), env, k) ->
      Eval (head, env, FunArg (((), arg), env, k))


  (* Evaluating the body of a let binding? Add the definition to the environment
     and evaluate the body. *)
  | Cont (LetBody ((x, (), body), env, k), def) ->
      Eval (body, (x, def) :: env, k)

  (* Evaluate the argument of a function application, applying it to the head of
     the application later on. *)
  | Cont (FunArg (((), arg), env, k), head) ->
      Eval (arg, env, FunApp ((head, ()), k))

  (* Continuing a function application? Add the argument to the environment of
     the closure and evaluate the body. *)
  | Cont (FunApp ((FunLit (Clos (env, x, body)), ()), k), arg) ->
      Eval (body, (x, arg) :: env, k)


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
