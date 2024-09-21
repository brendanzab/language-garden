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

type index = int
(** De Bruijn index, i.e. the number of binders between a variable occurance and
    the binder that it refers to. This allows for quick variable lookups in the
    environment without requiring names. *)

(* Primitive operations *)
type prim =
  | IntNeg
  | IntAdd
  | IntSub
  | IntMul

(** Lambda calculus expressions, with let bindings and integers *)
type expr =
  | Var of index                    (* variable occurences *)
  | Let of string * expr * expr     (* let bindings *)
  | FunLit of string * expr         (* function literals *)
  | FunApp of expr * expr           (* function applications *)
  | IntLit of int                   (* integer literal *)
  | PrimApp of prim * expr list     (* primitive applications *)


(** {1 Semantics} *)

(** {2 Semantic domain} *)

type value =
  | FunLit of string * clos         (* function literals *)
  | IntLit of int                   (* integer literal *)

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

  | PrimApp of (prim * value list * unit * expr list) * env * cont
  (** Evaluate the next argument in a primitive operation, applying the
      primitive if there are no arguments left to evaluate.

      Previously evaluated arguments are stored as values in reverse order,
      which are then provided to {!prim_app} when the primitive is applied.
  *)


(** The state of the abstract machine *)
type state =
  | Eval of expr * env * cont
  (** Evaluate an expression while deferring the continuation until later, where:

      - [expr] is the (C)ontrol instruction, to evaluate now
      - [env] is the (E)nvironment, to be used when evaluating the expression
      - [cont] is the (K)ontinuation, to be continued later with the computed value
  *)

  | Apply of cont * value
  (** Resume the continuation, plugging the “hole” with a value.

      Some presentations of the CEK machine get by with just the {!Eval} state
      (see Matt Might’s post linked above), but I found the transition rules
      were clearer if the application of the continuation was moved into a
      separate state, as is done by Felleisen and Friedman in Figure 2 of “The
      essence of compiling with continuations”.
  *)


(** Evaluate a primitive application, with the arguments in reverse order. *)
let prim_app (prim : prim) : value list -> value =
  match prim with
  | IntNeg -> fun[@warning "-partial-match"] [IntLit x] -> IntLit (-x)
  | IntAdd -> fun[@warning "-partial-match"] [IntLit y; IntLit x] -> IntLit (x + y)
  | IntSub -> fun[@warning "-partial-match"] [IntLit y; IntLit x] -> IntLit (x - y)
  | IntMul -> fun[@warning "-partial-match"] [IntLit y; IntLit x] -> IntLit (x * y)

(** Move the execution of the abstract machine forwards by one step. *)
let step (s : state) : state =
  match s with
  (* Evaluate a variable *)
  | Eval (Var index, env, k) ->
      Apply (k, List.nth env index)

  (* Evaluate a let binding *)
  | Eval (Let (name, def, body), env, k) ->
      Eval (def, env,                             (* evaluate the definition *)
        LetBody ((name, (), body), env, k))       (* continue evaluating the body later *)

  (* Evaluate a function literal *)
  | Eval (FunLit (name, body), env, k) ->
      Apply (k, FunLit (name, Clos (env, body)))

  (* Evaluate a function application *)
  | Eval (FunApp (head, arg), env, k) ->
      Eval (head, env,                            (* evaluate the head of the application *)
        FunArg (((), arg), env, k))               (* continue evaluating the argument later *)

  (* Evaluate an integer literal *)
  | Eval (IntLit n, env, k) ->
      Apply (k, IntLit n)

  (* Evaluate a primitive application with one-or-more arguments *)
  | Eval (PrimApp (prim, arg :: args), env, k) ->
      Eval (arg, env,                             (* evaluate the first argument now *)
        PrimApp ((prim, [], (), args), env, k))   (* evaluate the rest of the arguments later *)

  (* Evaluate a primitive application with zero arguments *)
  | Eval (PrimApp (prim, []), env, k) ->
      Apply (k, prim_app prim [])


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
      Eval (arg, env,                             (* evaluate the argument *)
        FunApp ((head, ()), k))                   (* continue applying the function later *)

  (* Resume applying a function, now that the head of the application and the
     argument have been evaluated *)
  | Apply (FunApp ((head, ()), k), arg) ->
      (*                   ▲        │
                           └────────┘ *)
      begin match head with
      | FunLit (_, Clos (env, body)) -> Eval (body, arg :: env, k)
      | _ -> invalid_arg "expected function literal"
      end

  (* Resume evaluating the next argument in a primitive application *)
  | Apply (PrimApp ((prim, rev_args, (), arg' :: args), env, k), arg) ->
      (*                             ▲                           │
                                     └───────────────────────────┘ *)
      Eval (arg', env,                                        (* evaluate the next argument now *)
        PrimApp ((prim, arg :: rev_args, (), args), env, k))  (* evaluate the rest of the arguments later *)

  (* Resume applying the primitive operation, now that all the arguments have
     been evaluated *)
  | Apply (PrimApp ((prim, rev_args, (), []), _, k), arg) ->
      Apply (k, prim_app prim (arg :: rev_args))


  (* Resume the empty continuation *)
  | Apply (Done, _) ->
      invalid_arg "cannot resume the empty continuation"


(** {2 Evaluation} *)

(** Compute the value of an expression *)
let eval (expr : expr) : value =
  (* Step the machine state until we reach the empty continuation.
     This is tail-recursive, so will not exhaust the stack when evaluating
     deeply nested expressions! *)
  let rec go (s : state) : value =
    match s with
    | Apply (Done, value) -> value
    | s -> (go [@tailcall]) (step s)
  in
  go (Eval (expr, [], Done))


(** {1 Tests} *)

(** Run with:

    {@command[
    $ ocaml scraps/eval-cek.ml
    ]}
*)

(** Interface for building well-scoped expressions *)
module Build : sig

  type t

  val let' : string * t -> (t -> t) -> t
  val fun' : string -> (t -> t) -> t
  val app : t -> t -> t
  val int : int -> t
  val prim_app : prim -> t list -> t

  val run : t -> expr

  (** Notation *)

  val ( let* ) : string * t -> (t -> t) -> t
  val ( $ ) : t -> t -> t
  val ( #$ ) : prim -> t list -> t

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
          upward  from the variable  occurance, as opposed to downwards from
          the top of the expression. *)
      Var (size - level - 1)

  let let' (name, def) (body : t -> t) : t =
    fun ~size ->
      Let (name, def ~size,
        body (var size) ~size:(size + 1))

  let fun' name (body : t -> t) : t =
    fun ~size ->
      FunLit (name,
        body (var size) ~size:(size + 1))

  let app (head : t) (arg : t) : t =
    fun ~size ->
      FunApp (head ~size, arg ~size)

  let int (i : int) : t =
    fun ~size ->
      IntLit i

  let prim_app (prim : prim) (args : t list) : t =
    fun ~size ->
      PrimApp (prim, List.map (fun arg -> arg ~size) args)

  let run (expr : t) : expr =
    expr ~size:0

  let ( let* ) = let'
  let ( $ ) = app
  let ( #$ ) = prim_app

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
