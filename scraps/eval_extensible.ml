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
module Extensible_variants = struct

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
module Polymorphic_variants = struct

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
      | `Int_lit of int
      | `Int_add of 'e expr * 'e expr
      | `Int_mul of 'e expr * 'e expr
      | `Int_eq of 'e expr * 'e expr
    ] as 'e

    type 'v value = [>
      | `Int_lit of int
      | `Bool_lit of bool
    ] as 'v

    let eval (eval : 'e -> 'v) (e : 'e expr) (default : unit -> 'v) : 'v value =
      let eval_int e =
        match eval e with
        | `Int_lit x -> x
        | _ -> failwith "expected integer"
      in
      match e with
      | `Int_lit i -> `Int_lit i
      | `Int_add (x, y) -> `Int_lit (eval_int x + eval_int y)
      | `Int_mul (x, y) -> `Int_lit (eval_int x * eval_int y)
      | `Int_eq (x, y) -> `Bool_lit (eval_int x = eval_int y)
      | _ -> default ()

  end

  module Bool = struct

    type 'e expr = [>
      | `Bool_lit of bool
      | `Elim of 'e expr * 'e expr * 'e expr
    ] as 'e

    type 'v value = [>
      | `Bool_lit of bool
    ] as 'v

    let eval (eval : 'e -> 'v) (e : 'e expr) (default : unit -> 'v) : 'v value =
      match e with
      | `Bool_lit b -> `Bool_lit b
      | `Bool_elim (head, body1, body2) ->
          begin match eval head with
          | `Bool_value true -> eval body1
          | `Bool_value false -> eval body2
          | _ -> failwith "expected boolean"
          end
      | _ -> default ()

  end

  module Fun = struct

    type 'e expr = [>
      | `Fun_lit of string * 'e expr
      | `Fun_app of 'e expr * 'e expr
    ] as 'e

    type 'v value = [>
      | `Fun_lit of ('v -> 'v)
    ] as 'v

    let eval (eval : 'v Env.t -> 'e -> 'v) (env : 'v Env.t) (e : 'e expr) (default : unit -> 'v) : 'v value =
      match e with
      | `Fun_lit (x, body) -> `Fun_lit (fun arg -> eval (Env.extend x arg env) body)
      | `Fun_app (head, arg) ->
          begin match eval env head with
          | `Fun_lit body -> body (eval env arg)
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
      `Let ("y", `Int_lit 3,
        `Fun_app (`Fun_lit ("x",
          `Int_add (`Var "x", `Int_lit 1)), `Var "y"))
    in

    assert (eval Env.empty e = `Int_lit 4);

  end

end
