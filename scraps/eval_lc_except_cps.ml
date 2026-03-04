(** An interpreter for the lambda calculus that supports the raising and
    handling of exceptions.

    Extends [eval_lc_cps.ml].

    {2 Resources}

    - https://www.cs.cornell.edu/courses/cs6110/2025sp/lectures/exceptions.pdf
*)

(** {1 Syntax} *)

type name = string

type index = int
(** De Bruijn index, i.e. the number of binders between a variable occurrence and
    the binder that it refers to. This allows for quick variable lookups in the
    environment without requiring names. *)

(** Lambda calculus expressions, with let bindings *)
type expr =
  | Var of index                          (* variable occurrences *)
  | Let of name * expr * expr             (* let bindings *)
  | Fun_lit of name * expr                (* function literals *)
  | Fun_app of expr * expr                (* function applications *)
  | String_lit of string                  (* string literals *)
  | Except_raise of expr                  (* raise an exception *)
  | Except_handle of expr * name * expr   (* handle an exception *)


(** {1 Semantics} *)

(** {2 Semantic domain} *)

type value =
  | Fun_lit of clos         (* functions *)
  | String_lit of string    (* strings *)

and clos =
  | Clos of env * expr

and env = value list

(** {2 Evaluation} *)

let ( let@ ) = ( @@ )

(** Evaluation function in continuation passing style *)
let rec eval : type a. env -> expr -> (value -> a) -> (value -> a) -> a =
  fun env expr h k ->
    match expr with
    | Var index ->
        k (List.nth env index)

    | Let (_, def, body) ->
        let@ def = eval env def h in
        eval (def :: env) body h k

    | Fun_lit (_, body) ->
        k (Fun_lit (Clos (env, body)))

    | Fun_app (head, arg) ->
        let@ head = eval env head h in
        let@ arg = eval env arg h in
        apply head arg h k

    | String_lit s ->
        k (String_lit s)

    | Except_raise arg ->
        let@ arg = eval env arg h in
        h arg

    | Except_handle (expr, _, body) ->
        eval env expr (fun arg -> eval (arg :: env) body h k) k

and apply : type a. value -> value -> (value -> a) -> (value -> a) -> a =
  fun head arg h k ->
    match head with
    | Fun_lit (Clos (env, body)) -> eval (arg :: env) body h k
    | _ -> failwith "apply"

(** Compute the value of an expression, returning an error if any unhandled
    exceptions were raised. *)
let eval (expr : expr) : (value, value) Result.t =
  eval [] expr Result.error Result.ok


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
  val string : string -> t
  val raise : t -> t
  val handle : t -> (string * (t -> t)) -> t

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
    fun ~size -> Fun_app (head ~size, arg ~size)

  let string (s : string) : t =
    fun ~size:_ -> String_lit s

  let raise (arg : t) : t =
    fun ~size -> Except_raise (arg ~size)

  let handle (expr : t) (name, body : string * (t -> t)) : t =
    fun ~size ->
      Except_handle (expr ~size, name, body (var size) ~size:(size + 1))

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

  (* Unhandled exception *)
  let () =
    let r = eval Build.(run begin
      let* id = "id", fun1 "x" Fun.id in
      let* const = "const", fun2 ("x", "y") Fun.const in
      (const $ raise (string "oops")) $ id $ id
    end) in
    assert (r = Error (String_lit "oops"))

  (* Handled exception *)
  let () =
    let r = eval Build.(run begin
      let* id = "id", fun1 "x" Fun.id in
      let* const = "const", fun2 ("x", "y") Fun.const in
      handle ((const $ raise (string "oops")) $ id $ id) ("msg", fun x -> id $ x)
    end) in
    assert (r = Ok (String_lit "oops"))

  (* Nested handlers *)
  let () =
    let r = eval Build.(run begin
      let* id = "id", fun1 "x" Fun.id in
      let* const = "const", fun2 ("x", "y") Fun.const in
      handle
        (handle ((const $ raise (string "oops")) $ id $ id) ("msg", fun x -> id $ x))
        ("msg", fun _ -> raise (string "ignored"))
    end) in
    assert (r = Ok (String_lit "oops"))

end
