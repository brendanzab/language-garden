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

    - {{: https://en.wikipedia.org/wiki/CEK_Machine} CEK Machine} on Wikipedia
    - {{: https://matt.might.net/articles/cek-machines/} Writing CEK-style interpreters (or semantics) in Haskell} by Matt Might
    - {{: https://github.com/kmicinski/cmsc330examples/blob/master/lambda-calculus/cek-lambda.ml} cek-lambda.ml} by Kristopher Micinski
    - {{: https://doi.org/10.1145/173262.155113} The essence of compiling with continuations} by Felleisen and Friedman
*)

(** {1 Syntax} *)

type index = int
(** De Bruijn index, i.e. the number of binders between a variable occurrence and
    the binder that it refers to. This allows for quick variable lookups in the
    environment without requiring names. *)

(** Lambda calculus expressions, with let bindings *)
type expr =
  | Var of index                    (* variable occurrences *)
  | Let of string * expr * expr     (* let bindings *)
  | Fun_lit of string * expr         (* function literals *)
  | Fun_app of expr * expr           (* function applications *)


(** {1 Semantics} *)

(** {2 Semantic domain} *)

type value =
  | Fun_lit of string * clos         (* function literals *)

and clos =
  | Clos of env * expr

and env = value list


(** {2 Abstract machine} *)

(** Continuation frame (i.e. a stack frame)

    This is an expression with a “hole” in it, marked by the [unit] type.
    It’s carefully crafted to result in a left-to-right, call-by-value
    evaluation strategy.
*)
type frame =
  | Let_body of (string * unit * expr) * env     (* let x := [.]; expr *)
  | Fun_app1 of (unit * expr) * env              (* [.] expr *)
  | Fun_app2 of (value * unit)                   (* value [.] *)

(** Continuation (i.e. a call-stack)

    This records a stack of “what to do next” after evaluating the current
    subexpression, represented as a linked list of continuation frames starting
    with the inner-most frame. The empty list signals that the expression has
    been fully evaluated.
*)
type cont = frame list


(** The state of the abstract machine *)
type state =
  | Eval of expr * env * cont
  (** Evaluate a subexpression in the current environment, deferring the rest of
      the expression until later by storing it in a continuation.

      This variant gives the CEK machine its name, where:

      - [expr] is the (C)ontrol instruction, to be evaluated next
      - [env] is the (E)nvironment, to be used when evaluating the subexpression
      - [cont] is the (K)ontinuation, to be continued later
  *)

  | Apply of cont * value
  (** Resume the continuation, plugging it with the value computed from the
      subexpression.

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
  | Eval (Var index, env, k) ->
      Apply (k, List.nth env index)

  (* Evaluate a let binding *)
  | Eval (Let (name, def, body), env, k) ->
      Eval (def, env,                         (* evaluate the definition *)
        Let_body ((name, (), body), env) :: k) (* continue evaluating the body later *)

  (* Evaluate a function literal *)
  | Eval (Fun_lit (name, body), env, k) ->
      Apply (k, Fun_lit (name, Clos (env, body)))

  (* Evaluate a function application *)
  | Eval (Fun_app (head, arg), env, k) ->
      Eval (head, env,                        (* evaluate the head of the application *)
        Fun_app1 (((), arg), env) :: k)        (* continue evaluating the argument later *)


  (* Resume evaluating the body of a let binding, now that the definition has
     been evaluated *)
  | Apply (Let_body ((_, (), body), env) :: k, def) ->
      (*                 ▲                     │
                         └─────────────────────┘ *)
      Eval (body, def :: env, k)

  (* Resume evaluating a function argument, now that the head of the
     application has been evaluated *)
  | Apply (Fun_app1 (((), arg), env) :: k, head) ->
      (*              ▲                    │
                      └────────────────────┘ *)
      Eval (arg, env,                         (* evaluate the argument *)
        Fun_app2 (head, ()) :: k)              (* continue applying the function later *)

  (* Resume applying a function, now that the head of the application and the
     argument have been evaluated *)
  | Apply (Fun_app2 (head, ()) :: k, arg) ->
      (*                   ▲         │
                           └─────────┘ *)
      begin match head with
      | Fun_lit (_, Clos (env, body)) -> Eval (body, arg :: env, k)
      end

  (* Resume the empty continuation *)
  | Apply ([], _) ->
      invalid_arg "cannot resume the empty continuation"


(** {2 Evaluation} *)

(** Compute the value of an expression *)
let eval (expr : expr) : value =
  (* Step the machine state until we reach the empty continuation.
     This is tail-recursive, so will not exhaust the stack when evaluating
     deeply nested expressions! *)
  let rec go (s : state) : value =
    match s with
    | Apply ([], value) -> value
    | s -> (go [@tailcall]) (step s)
  in
  go (Eval (expr, [], []))


(** {1 Tests} *)

(** Run with:

    {@command[
    $ ocaml scraps/eval-cek.ml
    ]}
*)

(** Interface for building well-scoped expressions *)
module Build : sig

  [@@@warning "-unused-value-declaration"]

  type t

  val let' : string * t -> (t -> t) -> t
  val fun' : string -> (t -> t) -> t
  val app : t -> t -> t

  val run : t -> expr

  (** Notation *)

  val ( let* ) : string * t -> (t -> t) -> t
  val ( $ ) : t -> t -> t

  val fun1 : string -> (t -> t) -> t
  val fun2 : (string * string) -> (t -> t -> t) -> t
  val fun3 : (string * string * string) -> (t -> t -> t -> t) -> t

end = struct

  type t = size:int -> expr
  (** Builds an expression where the [size] parameter keeps track of the number
      of variables currently bound in the environment. This is used when
      translating De Bruijn levels to De Bruijn indices. *)

  let var (level : int) : t =
    fun ~size ->
      (* Translate the level to an index, counting the number of binders
          upward  from the variable  occurrence, as opposed to downwards from
          the top of the expression. *)
      Var (size - level - 1)

  let let' (name, def) (body : t -> t) : t =
    fun ~size ->
      Let (name, def ~size,
        body (var size) ~size:(size + 1))

  let fun' name (body : t -> t) : t =
    fun ~size ->
      Fun_lit (name,
        body (var size) ~size:(size + 1))

  let app (head : t) (arg : t) : t =
    fun ~size ->
      Fun_app (head ~size, arg ~size)

  let run (expr : t) : expr =
    expr ~size:0

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

end

module Tests = struct

  let () =
    ignore @@ eval Build.(run begin
      fun1 "x" Fun.id $ fun1 "y" Fun.id
    end)

  let () =
    ignore @@ eval Build.(run begin
      let* id = "id", fun1 "x" Fun.id in
      let* const = "const", fun2 ("x", "y") Fun.const in
      (const $ id) $ id $ id
    end)

end
