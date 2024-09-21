(** A tree-walking interpreter for the lambda calculus, refactored into
    continuation-passing-style in the style of the CEK machine.

    Structuring evaluation in this way means that:

    - evaluation can be implemented tail-recursively, meaning the interpreter
      will not exhaust the stack when evaluating deeply nested expressions
    - evaluation can be stepped, which could be useful for debugging

    {2 The CEK machine}

    The CEK machine is an abstract machine that models the left-to-right,
    call-by-value lambda calculus. The name “CEK” is derived from the components
    of the machine state:

    {@text[
      ⟨ C, E, K ⟩
        ▲  ▲  ▲
        │  │  │
        │  │  The (K)ontinuation
        │  │
        │  The (E)nvironment
        │
        The (C)ontrol instruction
    ]}

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

    This is a list of {i evaluation contexts} that represents “what to do next”
    after evaluating the current expression. The continuation is carefully
    crafted to result in a left-to-right, call-by-value evaluation strategy.

    The [unit] type is used to mark the “hole’ in the continuation, i.e. the
    place where the resulting value will be substituted when applying the
    continuation.
*)
type cont =
  | Done
  (** The empty continuation *)

  | LetBody of (string * unit * expr) * env * cont
  (** Resume by evaluating the body of a let binding *)

  | FunArg of (unit * expr) * env * cont
  (** Resume by evaluating the argument of a function application *)

  | FunApp of (value * unit) * cont
  (** Resume by applying a function to an argument *)


(** The state of the abstract machine *)
type state =
  | Eval of expr * env * cont
  (** Evaluate an expression while deferring the continuation until later, where:

      - [expr] is the (C)ontrol instruction, to evaluate now
      - [env] is the (E)nvironment, to be used when evaluating the expression
      - [cont] is the (K)ontinuation, to be continued later with the computed value
  *)

  | Apply of cont * value
  (** Resume the continuation, pluging the “hole” with a value.

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
      Apply (k, List.nth env x)

  (* Evaluate a let binding *)
  | Eval (Let (x, def, body), env, k) ->
      Eval (def, env,                       (* evaluate the definition *)
        LetBody ((x, (), body), env, k))    (* continue evaluating the body later *)

  (* Evaluate a function literal *)
  | Eval (FunLit (x, body), env, k) ->
      Apply (k, FunLit (x, Clos (env, body)))

  (* Evaluate a function application *)
  | Eval (FunApp (head, arg), env, k) ->
      Eval (head, env,                      (* evaluate the head of the application *)
        FunArg (((), arg), env, k))         (* continue evaluating the argument later *)


  (* Resume evaluating the body of a let binding, now that the definition has
     been evaluated *)
  | Apply (LetBody ((_, (), body), env, k), def) ->
      (*                 ▲                   │
                         └───────────────────┘ *)
      Eval (body, def :: env, k)

  (* Resume evaluating a function argument, now that the head of the
     application has been evaluated *)
  | Apply (FunArg (((), arg), env, k), head) ->
      (*             ▲                  │
                     └──────────────────┘ *)
      Eval (arg, env,                       (* evaluate the argument *)
        FunApp ((head, ()), k))             (* continue applying the function later *)

  (* Resume applying a function, now that the head of the application and the
     argument have been evaluated *)
  | Apply (FunApp ((head, ()), k), arg) ->
      (*                   ▲        │
                           └────────┘ *)
      begin match head with
      | FunLit (_, Clos (env, body)) -> Eval (body, arg :: env, k)
      end

  (* Resume the empty continuation *)
  | Apply (Done, _) ->
      invalid_arg "cannot resume the empty continuation"


(** {2 Evaluation} *)

(** Compute the value of an expression *)
let eval (e : expr) : value =
  (* Step the machine state until we reach the empty continuation.
     This is tail-recursive, so will not exhaust the stack when evaluating
     deeply nested expressions! *)
  let rec go (s : state) : value =
    match s with
    | Apply (Done, v) -> v
    | s -> (go [@tailcall]) (step s)
  in
  go (Eval (e, [], Done))


(** {1 Tests} *)

module Tests = struct
  (** Run with:

      {@command[
      $ ocaml scraps/eval-cek.ml
      ]}
  *)

  module Build : sig

    type t

    val let' : string * t -> (t -> t) -> t
    val fun' : string -> (t -> t) -> t
    val app : t -> t -> t

    val ( let* ) : string * t -> (t -> t) -> t
    val ( $ ) : t -> t -> t

    val fun1 : string -> (t -> t) -> t
    val fun2 : (string * string) -> (t -> t -> t) -> t
    val fun3 : (string * string * string) -> (t -> t -> t -> t) -> t

    val run : t -> expr

  end = struct

    type t = size:int -> expr

    let var ~level : t =
      fun ~size ->
        Var (size - level - 1)

    let let' (name, def) (body : t -> t) : t =
      fun ~size ->
        Let (name, def ~size,
          body (var ~level:size) ~size:(size + 1))

    let fun' name (body : t -> t) : t =
      fun ~size ->
        FunLit (name,
          body (var ~level:size) ~size:(size + 1))

    let app (head : t) (arg : t) : t =
      fun ~size ->
        FunApp (head ~size, arg ~size)

    let ( let* ) = let'
    let ( $ ) = app

    let fun1 x body =
      fun' x body

    let fun2 (x, y) body =
      fun' x @@ fun x ->
      fun' y @@ fun y ->
        body x y

    let fun3 (x, y, z) body =
      fun' x @@ fun x ->
      fun' y @@ fun y ->
      fun' z @@ fun z ->
        body x y z

    let run (expr : t) : expr =
      expr ~size:0

  end

  let () = begin

    ignore @@ eval Build.(run begin
      fun1 "x" Fun.id $ fun1 "y" Fun.id
    end);

    ignore @@ eval Build.(run begin
      let* id = "id", fun1 "x" Fun.id in
      let* const = "const", fun2 ("x", "y") Fun.const in
      (const $ id) $ id $ id
    end);

  end

end
