(** A tree-walking interpreter for the lambda calculus, refactored into
    continuation-passing-style.

    We can later defunctionalise the evaluation function, which will result in
    the CEK machine (see [eval_lc_cek.ml]).
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

(** {2 Evaluation} *)

let ( let@ ) = ( @@ )

(** Evaluation function in continuation passing style *)
let rec eval_k (env : env) (expr : expr) (k : value -> value) : value =
  match expr with
  | Var index ->
      (k [@tailcall]) (List.nth env index)

  | Let (_, def, body) ->
      let@ def = eval_k env def in
      (eval_k [@tailcall]) (def :: env) body k

  | Fun_lit (name, body) ->
      (k [@tailcall]) (Fun_lit (name, Clos (env, body)))

  | Fun_app (head, arg) ->
      let@ head = eval_k env head in
      let@ arg = eval_k env arg in
      begin match head with
      | Fun_lit (_, Clos (env, body)) ->
          (eval_k [@tailcall]) (arg :: env) body k
      end

(** Compute the value of an expression *)
let eval (expr : expr) : value =
  eval_k [] expr Fun.id


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
