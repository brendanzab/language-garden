(** This is a demonstration of implementing interpreters in an extensible way
    that offers a partial solution to the expression problem. The idea is that
    the language can be extended with more features after the fact, without
    altering previous definitions. It also has the benefit of grouping together
    related extensions to the syntax and semantics in a per-feature way.

    This approach used is similar to the one described by Matthias Blume in
    {{:https://www.microsoft.com/en-us/research/video/records-sums-cases-and-exceptions-row-polymorphism-at-work/}
    Records, sums, cases, and exceptions: Row-polymorphism at work}.
*)

[@@@warning "-unused-extension"]


(** An extensible interpreter implemented using OCaml’s extensible variant
    types. See the {{:https://ocaml.org/manual/5.2/extensiblevariants.html}
    OCaml manual}. for more information on this feature. *)
module ExtensibleVariants = struct

  (** Type of expressions *)
  type expr = ..

  (** Type of values *)
  type value = ..


  (** Environments, as mappings from strings to values *)
  module Env = struct

    type t = string -> value

    let empty : t =
      fun x -> failwith ("unbound variable `" ^ x ^ "`")

    let extend (x : string) (value : value) (env : t) : t =
      fun x' -> if x = x' then value else env x

  end


  (* Language extensions *)

  module Bool = struct

    type expr +=
      | Lit of bool
      | Elim of expr * expr * expr

    type value +=
      | Value of bool

    let eval (eval : expr -> value) (e : expr) (default : unit -> value) : value =
      match e with
      | Lit b -> Value b
      | Elim (head, body1, body2) ->
          begin match eval head with
          | Value true -> eval body1
          | Value false -> eval body2
          | _ -> failwith "expected boolean"
          end
      | _ -> default ()

  end

  module Int = struct

    type expr +=
      | Lit of int
      | Add of expr * expr
      | Mul of expr * expr
      | Eq of expr * expr

    type value +=
      | Value of int

    let eval (eval : expr -> value) (e : expr) (default : unit -> value) : value =
      let eval_int e =
        match eval e with
        | Value x -> x
        | _ -> failwith "expected integer"
      in
      match e with
      | Lit i -> Value i
      | Add (x, y) -> Value (eval_int x + eval_int y)
      | Mul (x, y) -> Value (eval_int x * eval_int y)
      | Eq (x, y) -> Bool.Value (eval_int x = eval_int y)
      | _ -> default ()

  end

  module Fun = struct

    type expr +=
      | Lit of string * expr
      | App of expr * expr

    type value +=
      | Value of (value -> value)

    let eval (eval : Env.t -> expr -> value) (env : Env.t) (e : expr) (default : unit -> value) : value =
      match e with
      | Lit (x, body) -> Value (fun arg -> eval (Env.extend x arg env) body)
      | App (head, arg) ->
          begin match eval env head with
          | Value body -> body (eval env arg)
          | _ -> failwith "expected function"
          end
      | _ -> default ()

  end

  module Var = struct

    type expr +=
      | Var of string

    let eval (env : Env.t) (e : expr) (default : unit -> value) : value =
      match e with
      | Var x -> env x
      | _ -> default ()

  end

  module Let = struct

    type expr +=
      | Let of string * expr * expr

    let eval (eval : Env.t -> expr -> value) (env : Env.t) (e : expr) (default : unit -> value) : value =
      match e with
      | Let (x, def, body) -> eval (Env.extend x (eval env def) env) body
      | _ -> default ()

  end


  (* Wiring it all together! *)

  let ( let@ ) = ( @@ )

  let rec eval (env : Env.t) (e : expr) : value =
    let@ () = Int.eval (eval env) e in
    let@ () = Bool.eval (eval env) e in
    let@ () = Fun.eval eval env e in
    let@ () = Var.eval env e in
    let@ () = Let.eval eval env e in
    failwith "expression not covered"


  (* Tests *)

  let () = begin

    let e : expr =
      Let.Let ("y", Int.Lit 3,
        Fun.App (Fun.Lit ("x",
          Int.Add (Var.Var "x", Int.Lit 1)), Var.Var "y"))
    in

    assert (eval Env.empty e = Int.Value 4);

  end

end


(** An extensible interpreter implemented using OCaml’s polymorphic variants.
    See the {{:https://ocaml.org/manual/5.2/polyvariant.html} OCaml manual}.
    for more information on this feature. *)
module PolymorphicVariants = struct

  (** Environments, as mappings from strings to values *)
  module Env = struct

    type 'v t = string -> 'v

    let empty (type v) : v t =
      fun x -> failwith ("unbound variable `" ^ x ^ "`")

    let extend (type v) (x : string) (value : v) (env : v t) : v t =
      fun x' -> if x = x' then value else env x

  end


  (* Language extensions *)

  module Int = struct

    type 'e expr = [>
      | `IntLit of int
      | `IntAdd of 'e expr * 'e expr
      | `IntMul of 'e expr * 'e expr
      | `IntEq of 'e expr * 'e expr
    ] as 'e

    type 'v value = [>
      | `IntLit of int
      | `BoolLit of bool
    ] as 'v

    let eval (eval : 'e -> 'v) (e : 'e expr) (default : unit -> 'v) : 'v value =
      let eval_int e =
        match eval e with
        | `IntLit x -> x
        | _ -> failwith "expected integer"
      in
      match e with
      | `IntLit i -> `IntLit i
      | `IntAdd (x, y) -> `IntLit (eval_int x + eval_int y)
      | `IntMul (x, y) -> `IntLit (eval_int x * eval_int y)
      | `IntEq (x, y) -> `BoolLit (eval_int x = eval_int y)
      | _ -> default ()

  end

  module Bool = struct

    type 'e expr = [>
      | `BoolLit of bool
      | `Elim of 'e expr * 'e expr * 'e expr
    ] as 'e

    type 'v value = [>
      | `BoolLit of bool
    ] as 'v

    let eval (eval : 'e -> 'v) (e : 'e expr) (default : unit -> 'v) : 'v value =
      match e with
      | `BoolLit b -> `BoolLit b
      | `BoolElim (head, body1, body2) ->
          begin match eval head with
          | `BoolValue true -> eval body1
          | `BoolValue false -> eval body2
          | _ -> failwith "expected boolean"
          end
      | _ -> default ()

  end

  module Fun = struct

    type 'e expr = [>
      | `FunLit of string * 'e expr
      | `FunApp of 'e expr * 'e expr
    ] as 'e

    type 'v value = [>
      | `FunLit of ('v -> 'v)
    ] as 'v

    let eval (eval : 'v Env.t -> 'e -> 'v) (env : 'v Env.t) (e : 'e expr) (default : unit -> 'v) : 'v value =
      match e with
      | `FunLit (x, body) -> `FunLit (fun arg -> eval (Env.extend x arg env) body)
      | `FunApp (head, arg) ->
          begin match eval env head with
          | `FunLit body -> body (eval env arg)
          | _ -> failwith "expected function"
          end
      | _ -> default ()

  end

  module Var = struct

    type 'e expr = [>
      | `Var of string
    ] as 'e

    let eval (env : 'v Env.t) (e : 'e expr) (default : unit -> 'v) : 'v =
      match e with
      | `Var x -> env x
      | _ -> default ()

  end

  module Let = struct

    type 'e expr = [>
      | `Let of string * 'e * 'e
    ] as 'e

    let eval (eval : 'v Env.t -> 'e -> 'v) (env : 'v Env.t) (e : 'e expr) (default : unit -> 'v) : 'v =
      match e with
      | `Let (x, def, body) -> eval (Env.extend x (eval env def) env) body
      | _ -> default ()

  end


  (* Wiring it all together! *)

  type expr = expr Var.expr Let.expr Fun.expr Bool.expr Int.expr
  type value = value Fun.value Bool.value Int.value

  let ( let@ ) = ( @@ )

  let rec eval (env : value Env.t) (e : expr) : value =
    let@ () = Int.eval (eval env) e in
    let@ () = Bool.eval (eval env) e in
    let@ () = Fun.eval eval env e in
    let@ () = Var.eval env e in
    let@ () = Let.eval eval env e in
    failwith "expression not covered"


  (* Tests *)

  let () = begin

    let e : expr =
      `Let ("y", `IntLit 3,
        `FunApp (`FunLit ("x",
          `IntAdd (`Var "x", `IntLit 1)), `Var "y"))
    in

    assert (eval Env.empty e = `IntLit 4);

  end

end
