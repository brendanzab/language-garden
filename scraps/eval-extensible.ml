(** This is a demonstration of implementing interpreters in an extensible way
    that offers a partial solution to the expression problem. The idea is that
    the language can be extended with more features after the fact, without
    altering previous definitions. It also has the benefit of grouping together
    related extensions to the syntax and semantics in a per-feature way.

    This approach used is similar to the one described by Matthias Blume in
    {{:https://www.microsoft.com/en-us/research/video/records-sums-cases-and-exceptions-row-polymorphism-at-work/}
    Records, sums, cases, and exceptions: Row-polymorphism at work}.
*)

module ExtensibleVariants = struct

  type expr = ..
  type value = ..

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
      | Lit i -> Value i
      | Elim (b, body1, body2) ->
          begin match eval b with
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
      let apply2 f v1 v2 =
        match v1, v2 with
        | Value x, Value y -> Value (f x y)
        | _ -> failwith "expected integers"
      in
      match e with
      | Lit i -> Value i
      | Add (x, y) -> apply2 ( + ) (eval x) (eval y)
      | Mul (x, y) -> apply2 ( * ) (eval x) (eval y)
      | Eq (x, y) ->
          begin match eval x, eval y with
          | Value x, Value y -> Bool.Value (x = y)
          | _ -> failwith "expected integers"
          end
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
      | Lit (x, b) -> Value (fun arg -> eval (Env.extend x arg env) b)
      | App (f, arg) ->
          begin match eval env f with
          | Value f -> f (eval env arg)
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
      | Let (x, e, b) -> eval (Env.extend x (eval env e) env) b
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


module PolymorphicVariants = struct

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
      let apply2 f v1 v2 =
        match v1, v2 with
        | `IntLit x, `IntLit y -> `IntLit (f x y)
        | _ -> failwith "expected integers"
      in
      match e with
      | `IntLit i -> `IntLit i
      | `IntAdd (x, y) -> apply2 ( + ) (eval x) (eval y)
      | `IntMul (x, y) -> apply2 ( * ) (eval x) (eval y)
      | `IntEq (x, y) ->
          begin match eval x, eval y with
          | `IntLit x, `IntLit y -> `BoolValue (x = y)
          | _ -> failwith "expected integers"
          end
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
      | `BoolLit i -> `BoolLit i
      | `BoolElim (b, body1, body2) ->
          begin match eval b with
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
      | `FunLit (x, b) -> `FunLit (fun arg -> eval (Env.extend x arg env) b)
      | `FunApp (f, arg) ->
          begin match eval env f with
          | `FunLit f -> f (eval env arg)
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
      | `Let (x, e, b) -> eval (Env.extend x (eval env e) env) b
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
