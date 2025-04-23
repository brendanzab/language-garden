(** A well-typed lambda calculus evaluator using GADTs. *)

type 'ctx env =
  | [] : unit env
  | ( :: ) : 'a * 'ctx env -> ('a * 'ctx) env

type ('ctx, 'a) var =
  | Stop : ('a * 'ctx, 'a) var
  | Pop : ('ctx, 'a) var -> ('b * 'ctx, 'a) var

type ('ctx, 'a) expr =
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Var : ('ctx, 'a) var -> ('ctx, 'a) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr

let rec lookup : type ctx a. (ctx, a) var -> ctx env -> a =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> lookup x env

let rec eval : type ctx a. ctx env -> (ctx, a) expr -> a =
  fun env expr ->
    match expr with
    | Let (def, body) -> eval (eval env def :: env) body
    | Var x -> lookup x env
    | Fun_abs body -> fun x -> eval (x :: env) body
    | Fun_app (fn, arg) -> (eval env fn) (eval env arg)

let () = begin

  print_string "Running tests ...";

  assert (eval [] (Fun_abs (Var Stop)) 1 = 1);
  assert (eval [] (Fun_abs (Fun_abs (Var (Pop Stop)))) "hello" 4 = "hello");
  assert (eval ["hello"] (Fun_app (Fun_abs (Fun_abs (Var (Pop Stop))), Var Stop)) 4 = "hello");
  assert (eval [2; "hello"] (Let (Var (Pop Stop), Var Stop)) = "hello");

  print_string " ok!\n";

end
