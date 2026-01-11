(** A well-typed lambda calculus evaluator with an optimised representation of
    de Bruijn indices.

    Extends [eval_stlc_gadt] and [misc_nat_int_repr].
*)

module Index : sig

  type ('ctx, 'a) t [@@immediate]

  val stop : ('a * 'ctx, 'a) t
  val pop : ('ctx, 'a) t -> ('b * 'ctx, 'a) t

  type ('ctx, 'a) layer =
    | Stop : ('a * 'ctx, 'a) layer
    | Pop : ('ctx, 'a) t -> ('b * 'ctx, 'a) layer

  val elim : ('ctx, 'a) t -> ('ctx, 'a) layer

end = struct

  type ('ctx, 'a) t = int

  let stop = 0

  let pop i =
    if i <> Int.max_int then i + 1 else
      invalid_arg "Index.pop"

  type ('ctx, 'a) layer =
    | Stop : ('a * 'ctx, 'a) layer
    | Pop : ('ctx, 'a) t -> ('b * 'ctx, 'a) layer

  let elim : type ctx a. (ctx, a) t -> (ctx, a) layer =
    function
    | 0 -> Obj.magic Stop
    | i -> Obj.magic (Pop (i - 1))

end

type ('ctx, 'a) expr =
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Var : ('ctx, 'a) Index.t -> ('ctx, 'a) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr

type 'ctx env =
  | [] : unit env
  | ( :: ) : 'a * 'ctx env -> ('a * 'ctx) env

let rec lookup : type ctx a. (ctx, a) Index.t -> ctx env -> a =
  fun x env ->
    match Index.elim x, env with
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

  Printf.printf "Running tests in %s ..." __FILE__;

  assert (eval [] (Fun_abs (Var Index.stop)) 1 = 1);
  assert (eval [] (Fun_abs (Fun_abs (Var Index.(pop stop)))) "hello" 4 = "hello");
  assert (eval ["hello"] (Fun_app (Fun_abs (Fun_abs (Var Index.(pop stop))), Var Index.stop)) 4 = "hello");
  assert (eval [2; "hello"] (Let (Var Index.(pop stop), Var Index.stop)) = "hello");

  Printf.printf " ok!\n";

end
