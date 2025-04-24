(** A well-typed lambda calculus evaluator, extended with some global
    definitions. Extends [eval_stlc_gadt]. *)

type 'a global_var =
  | Zero : int global_var
  | Id : (int -> int) global_var
  | Const : (int -> string -> int) global_var
  | Fix : (((int -> int) -> int -> int) -> int -> int) global_var

type ('ctx, 'a) index =
  | Stop : ('a * 'ctx, 'a) index
  | Pop : ('ctx, 'a) index -> ('b * 'ctx, 'a) index

type ('ctx, 'a) expr =
  | Global_var : 'a global_var -> ('ctx, 'a) expr
  | Local_var : ('ctx, 'a) index -> ('ctx, 'a) expr
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr
  | Int_lit : int -> ('ctx, int) expr
  | String_lit : string -> ('ctx, string) expr

let lookup_global : type ctx a. a global_var -> (ctx, a) expr  =
  let ( $ ) f x = Fun_app (f, x) in
  function
  | Zero -> Int_lit 0
  | Id -> Fun_abs (Local_var Stop)
  | Const -> Fun_abs (Fun_abs (Local_var (Pop Stop)))
  | Fix ->
      let fix = Global_var Fix in
      Fun_abs (* f *) (Fun_abs (* x *) (
        let f = Local_var (Pop Stop) in
        let x = Local_var Stop in
        f $ (fix $ f) $ x))

type 'ctx env =
  | [] : unit env
  | ( :: ) : 'a * 'ctx env -> ('a * 'ctx) env

let rec lookup : type ctx a. (ctx, a) index -> ctx env -> a =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> lookup x env

let rec eval : type ctx a. ctx env -> (ctx, a) expr -> a =
  fun env ->
    function
    | Global_var x -> eval [] (lookup_global x)
    | Let (def, body) -> eval (eval env def :: env) body
    | Local_var x -> lookup x env
    | Fun_abs body -> fun x -> eval (x :: env) body
    | Fun_app (fn, arg) -> (eval env fn) (eval env arg)
    | Int_lit i -> i
    | String_lit s -> s

let () = begin
  let ( $ ) f x = Fun_app (f, x) in

  print_string "Running tests ...";

  assert (eval [] (Fun_abs (Local_var Stop)) 1 = 1);
  assert (eval [] (Fun_abs (Fun_abs (Local_var (Pop Stop)))) "hello" 4 = "hello");
  assert (eval ["hello"] (Fun_abs (Fun_abs (Local_var (Pop Stop))) $ Local_var Stop) 4 = "hello");
  assert (eval [2; "hello"] (Let (Local_var (Pop Stop), Local_var Stop)) = "hello");
  assert (eval [] (Global_var Id $ Global_var Zero) = 0);
  assert (eval [] (Global_var Const $ Global_var Zero $ String_lit "hello") = 0);

  print_string " ok!\n";

end
