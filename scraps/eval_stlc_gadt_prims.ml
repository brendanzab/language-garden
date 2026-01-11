(** A well-typed lambda calculus evaluator using GADTs, extended with some
    primitive operations.

    Extends [eval_stlc_gadt].
*)

[@@@warning "-unused-constructor"]

type ('ctx, 'a) index =
  | Stop : ('a * 'ctx, 'a) index
  | Pop : ('ctx, 'a) index -> ('b * 'ctx, 'a) index

type 'a prim =
  | Int_eq : (int -> int -> bool) prim
  | Int_add : (int -> int -> int) prim
  | Int_sub : (int -> int -> int) prim
  | Int_mul : (int -> int -> int) prim
  | Int_neg : (int -> int) prim
  | String_eq : (string -> string -> bool) prim
  | String_cat : (string -> string -> string) prim

type ('ctx, 'a) expr =
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Var : ('ctx, 'a) index -> ('ctx, 'a) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr
  | Int_lit : int -> ('ctx, int) expr
  | String_lit : string -> ('ctx, string) expr
  | Prim : 'a prim -> ('ctx, 'a) expr

type 'ctx env =
  | [] : unit env
  | ( :: ) : 'a * 'ctx env -> ('a * 'ctx) env

let rec eval_index : type ctx a. (ctx, a) index -> ctx env -> a =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> eval_index x env

let eval_prim : type a. a prim -> a =
  fun p ->
    match p with
    | Int_eq -> fun i1 i2 -> i1 = i2
    | Int_add -> fun i1 i2 -> i1 + i2
    | Int_sub -> fun i1 i2 -> i1 - i2
    | Int_mul -> fun i1 i2 -> i1 * i2
    | Int_neg -> fun i -> -i
    | String_eq -> fun s1 s2 -> s1 = s2
    | String_cat -> fun s1 s2 -> s1 ^ s2

let rec eval_expr : type ctx a. ctx env -> (ctx, a) expr -> a =
  fun env expr ->
    match expr with
    | Let (def, body) -> eval_expr (eval_expr env def :: env) body
    | Var x -> eval_index x env
    | Fun_abs body -> fun x -> eval_expr (x :: env) body
    | Fun_app (fn, arg) -> (eval_expr env fn) (eval_expr env arg)
    | Int_lit i -> i
    | String_lit s -> s
    | Prim prim -> eval_prim prim

let () = begin

  Printf.printf "Running tests in %s ..." __FILE__;

  assert (eval_expr [] (Fun_abs (Var Stop)) 1 = 1);
  assert (eval_expr [] (Fun_abs (Fun_abs (Var (Pop Stop)))) "hello" 4 = "hello");
  assert (eval_expr [] (Fun_app (Fun_abs (Fun_abs (Var (Pop Stop))), String_lit "hello")) 4 = "hello");
  assert (eval_expr [] (Fun_app (Fun_app (Prim String_cat, String_lit "hello"), String_lit " world!")) = "hello world!");

  Printf.printf " ok!\n";

end
