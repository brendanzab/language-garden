(** A well-typed lambda calculus evaluator, with a separate value datatype, this
    time using defunctionalised closures in the semantic domain, as opposed to
    HOAS.

    Extends [eval_stlc_gadt_values].
*)

type ('ctx, 'a) index =
  | Stop : ('a * 'ctx, 'a) index
  | Pop : ('ctx, 'a) index -> ('b * 'ctx, 'a) index

type ('ctx, 'a) expr =
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Var : ('ctx, 'a) index -> ('ctx, 'a) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr
  | Int_lit : int -> ('ctx, int) expr
  | String_lit : string -> ('ctx, string) expr

type 'a value =
  | Fun_abs : ('a, 'b) clos -> ('a -> 'b) value
  | Int_lit : int -> int value
  | String_lit : string -> string value

and ('a, 'b) clos =
  | Clos : 'ctx env * ('a * 'ctx, 'b) expr -> ('a, 'b) clos

and 'ctx env =
  | [] : unit env
  | ( :: ) : 'a value * 'ctx env -> ('a * 'ctx) env

let rec lookup : type ctx a. (ctx, a) index -> ctx env -> a value =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> lookup x env

let rec eval : type ctx a. ctx env -> (ctx, a) expr -> a value =
  fun env expr ->
    match expr with
    | Let (def, body) -> eval (eval env def :: env) body
    | Var x -> lookup x env
    | Fun_abs body -> Fun_abs (Clos (env, body))
    | Fun_app (fn, arg) ->
        let Fun_abs clos = eval env fn in
        clos_app clos (eval env arg)
    | Int_lit i -> Int_lit i
    | String_lit s -> String_lit s

and clos_app : type a b. (a, b) clos -> a value -> b value =
  fun (Clos (env, body)) arg ->
    eval (arg :: env) body

let () = begin

  Printf.printf "Running tests in %s ..." __FILE__;

  assert (eval [] (Fun_app (Fun_abs (Var Stop), Int_lit 1)) = Int_lit 1);
  assert (eval [] (Fun_app (Fun_app (Fun_abs (Fun_abs (Var (Pop Stop))), String_lit "hello"), Int_lit 4)) = String_lit "hello");
  assert (eval [String_lit "hello"] (Fun_app (Fun_app (Fun_abs (Fun_abs (Var (Pop Stop))), Var Stop), Int_lit 4)) = String_lit "hello");
  assert (eval [Int_lit 2; String_lit "hello"] (Let (Var (Pop Stop), Var Stop)) = String_lit "hello");

  Printf.printf " ok!\n";

end
