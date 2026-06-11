(** A tree-walking interpreter for the lambda calculus, refactored into
    continuation-passing-style.

    This approach is useful for implementing the interpreter in a tail-recursive
    fashion, and can serve as the basis for implementing non-local control flow
    (see [eval_lc_except_cps.ml]).

    We can later defunctionalise the evaluation function, which will result in
    the CEK machine (see [eval_lc_cek.ml]). Another option is to
    {{: https://en.wikipedia.org/wiki/Partial_evaluation#Futamura_projections}
    specialise} the interpreter to yield a compiler to a CPS-based IR.
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
  | Fun_lit of string * expr        (* function literals *)
  | Fun_app of expr * expr          (* function applications *)


(** {1 Semantics} *)

(** {2 Semantic domain} *)

type value =
  | Fun_lit of clos         (* functions *)

and clos =
  | Clos of env * expr

and env = value list

(** {2 Evaluation} *)

(** A {{: https://ocaml.org/manual/5.4/bindingops.html} binding operator} that
    applies a continuation to an intermediate computation. This allows us to
    replace continuation applications:

    {[
      eval env head (fun head ->
        eval env arg (fun arg ->
          apply head arg return))
    ]}

    With the following notation:

    {[
      let@ head = eval env head in
      let@ arg = eval env arg in
      apply head arg return
    ]}

    This is equivalent to function application, but we use a more precise type
    for clarity.
*)
let ( let@ ) : type a. ((value -> a) -> a) -> (value -> a) -> a =
  ( @@ )

(** Evaluation function in continuation passing style *)
let rec eval : type a. env -> expr -> (value -> a) -> a =
  fun env expr return ->
    match expr with
    | Var index ->
        return (List.nth env index)

    | Let (_, def, body) ->
        let@ def = eval env def in
        eval (def :: env) body return

    | Fun_lit (_, body) ->
        return (Fun_lit (Clos (env, body)))

    | Fun_app (head, arg) ->
        let@ head = eval env head in
        let@ arg = eval env arg in
        apply head arg return

and apply : type a. value -> value -> (value -> a) -> a =
  fun head arg return ->
    match head with
    | Fun_lit (Clos (env, body)) ->
        eval (arg :: env) body return

(** Compute the value of an expression *)
let eval (expr : expr) : value =
  eval [] expr Fun.id


(** {1 Tests} *)

(** Interface for building well-scoped expressions *)
module Build : sig

  [@@@warning "-unused-value-declaration"]

  type t

  val let' : string * t -> (t -> t) -> t
  val fun' : string -> (t -> t) -> t
  val app : t -> t -> t

  val run : t -> expr

  (** Notation *)

  val ( let$ ) : string * t -> (t -> t) -> t
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

  let ( let$ ) = let'
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
      let$ id = "id", fun1 "x" Fun.id in
      let$ const = "const", fun2 ("x", "y") Fun.const in
      (const $ id) $ id $ id
    end)

end
