(** An interpreter for the lambda calculus, implemented using the CEK machine.

    The CEK machine is an approach for modelling the left-to-right,
    call-by-value lambda calculus in terms of an abstract machine. The name
    “CEK” is derived from the components of the machine state:

    {v
      ⟨ C, E, K ⟩
        ▲  ▲  ▲
        │  │  │
        │  │  The (K)ontinuation
        │  │
        │  The (E)nvironment
        │
        The (C)ontrol instruction
    v}

    By structuring evaluation in this way, it means that:

    - evaluation can be implemented tail-recursively
    - the interpreter can be stepped, which could be useful for debugging

    {2 Resources}

    - {{:https://en.wikipedia.org/wiki/CEK_Machine} CEK Machine} on Wikipedia
    - {{:https://matt.might.net/articles/cek-machines/} Writing CEK-style interpreters (or semantics) in Haskell} by Matt Might
    - {{:https://github.com/kmicinski/cmsc330examples/blob/master/lambda-calculus/cek-lambda.ml} cek-lambda.ml} by Kristopher Micinski
    - {{:https://doi.org/10.1145/173262.155113} The essence of compiling with continuations} by Felleisen and Friedman
*)

(** {1 Syntax} *)

(** Lambda calculus expressions, with let bindings *)
type expr =
  | Var of int                      (* variable occurences (as de Bruijn indices) *)
  | Let of string * expr * expr     (* let bindings *)
  | FunLit of string * expr         (* function literals *)
  | FunApp of expr * expr           (* function applications *)


(** {1 Semantics} *)

(** {2 Semantic domain} *)

type value =
  | FunLit of string * clos         (* function literals *)

and clos =
  | Clos of env * expr

and env = value list


(** {2 Abstract machine} *)

(** Defunctionalised continuation

    The continuation is carefully crafted to result in a left-to-right,
    call-by-value evaluation strategy.

    The [unit] type is used to mark the “hole” in the continuation, where a
    value will be substituted when applying the continuation.
*)
type cont =
  | Done
  (** The empty continuation *)

  | LetBody of (string * unit * expr) * env * cont
  (** Evaluate the body of a let binding *)

  | FunArg of (unit * expr) * env * cont
  (** Evaluate the argument of a function application *)

  | FunApp of (value * unit) * cont
  (** Apply a function to an argument *)


(** The state of the abstract machine *)
type state =
  | Eval of expr * env * cont
  (** Evaluate an expression while deferring the continuation until later, where:

      - [expr] is the (C)ontrol instruction, to evaluate now
      - [env] is the (E)nvironment, to be used when evaluating the expression
      - [cont] is the (K)ontinuation, to be continued later with the computed value
  *)

  | Cont of cont * value
  (** Plug the “hole” in a continuation with a value.

      Some presentations of the CEK machine get by with just the [Eval] state
      (see Matt Might’s post linked above), but I found the transition rules
      were clearer if the application of the continuation was moved into a
      separate state, as is done by Felleisen and Friedman in Figure 2 of “The
      essence of compiling with continuations”.
  *)


(** Move the execution of the abstract machine forwards by one step. *)
let step (s : state) : state =
  match s with
  (* Evaluate a variable *)
  | Eval (Var x, env, k) ->
      Cont (k, List.nth env x)

  (* Evaluate a let binding *)
  | Eval (Let (x, def, body), env, k) ->
      Eval (def, env,                       (* evaluate the definition *)
        LetBody ((x, (), body), env, k))    (* continue evaluating the body later *)

  (* Evaluate a function literal *)
  | Eval (FunLit (x, body), env, k) ->
      Cont (k, FunLit (x, Clos (env, body)))

  (* Evaluate a function application *)
  | Eval (FunApp (head, arg), env, k) ->
      Eval (head, env,                      (* evaluate the head of the application *)
        FunArg (((), arg), env, k))         (* continue evaluating the argument later *)


  (* Continue evaluating the body of a let binding, now that the definition has
     been evaluated *)
  | Cont (LetBody ((_, (), body), env, k), def) ->
      (*                ▲                   │
                        └───────────────────┘ *)
      Eval (body, def :: env, k)

  (* Continue evaluating a function argument, now that the head of the
     application has been evaluated *)
  | Cont (FunArg (((), arg), env, k), head) ->
      (*            ▲                  │
                    └──────────────────┘ *)
      Eval (arg, env,                       (* evaluate the argument *)
        FunApp ((head, ()), k))             (* continue applying the function later *)

  (* Continue applying a function, now that the head of the application and the
     argument have been evaluated *)
  | Cont (FunApp ((head, ()), k), arg) ->
      (*                  ▲        │
                          └────────┘ *)
      begin match head with
      | FunLit (_, Clos (env, body)) -> Eval (body, arg :: env, k)
      end

  (* Continue the empty continuation *)
  | Cont (Done, _) ->
      invalid_arg "cannot continue the empty continuation"


(** {2 Evaluation} *)

(** Compute the value of an expression *)
let eval (e : expr) : value =
  (* Step the machine state until we reach the empty continuation *)
  let rec go (s : state) : value =
    match s with
    | Cont (Done, v) -> v
    | s -> (go [@tailcall]) (step s)
  in
  go (Eval (e, [], Done))
