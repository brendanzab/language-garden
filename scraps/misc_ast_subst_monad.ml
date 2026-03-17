(** Variable Substitution as a monad

    http://blog.sigfpe.com/2006/11/variable-substitution-gives.html
*)

type 'a expr =
  | Var of 'a
  | Int of int
  | Add of 'a expr * 'a expr
  | Mul of 'a expr * 'a expr

let [@warning "-unused-value-declaration"] pure : type a. a -> a expr =
  fun x -> Var x

let rec bind : type a b. a expr -> (a -> b expr) -> b expr =
  fun e f ->
    match e with
    | Var x -> f x
    | Int n -> Int n
    | Add (a, b) -> Add (bind a f, bind b f)
    | Mul (a, b) -> Mul (bind a f, bind b f)

let ( let* ) = bind

let rec subst : type a. (a -> a expr) -> a expr -> a expr =
  fun env e ->
    let* a = e in
    subst env (env a)

let env : string -> string expr =
  function
  | "a" -> Int 1
  | "b" -> Add (Var "a", Int 2)
  | "c" -> Add (Var "a", Var "b")
  | n -> Var n (* FIXME: Could cause subst to loop forever *)

let () =
  assert (subst env (Mul (Var "a", Var "c")) =
    Mul (Int 1, Add (Int 1, Add (Int 1, Int 2))));
