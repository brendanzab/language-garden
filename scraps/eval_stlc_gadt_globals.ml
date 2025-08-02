(** A well-typed lambda calculus evaluator, extended with some global
    definitions.

    Extends [eval_stlc_gadt].

    Resources:

    - Kiran Gopinathan, {{: https://discuss.ocaml.org/t/best-approach-for-implementing-open-recursion-over-extensible-types/11678}
      Best approach for implementing open recursion over extensible types}
    - Gaëtan Gilbert, {{: https://discuss.ocaml.org/t/playing-with-extensible-type/16613/9}
      Comment on Playing with extensible type}
*)

module Global : sig

  type 'a t

  val define : 'a -> 'a t
  val lookup : 'a t -> 'a

end = struct

  type 'a t = ..

  type def = {
    run : 'a. 'a t -> 'a option;
  }

  let defs : def Dynarray.t = Dynarray.create ()

  let define (type a) (v : a) : a t =
    let open struct
      type _ t += Global : a t
    end in
    Dynarray.add_last defs {
      run = fun (type a) (g : a t) : a option ->
        match g with
        | Global -> Some v
        | _ -> None;
    };
    Global

  let lookup : type a. a t -> a = fun g ->
    Dynarray.find_map (fun f -> f.run g) defs
    |> Option.get

end

type ('ctx, 'a) index =
  | Stop : ('a * 'ctx, 'a) index
  | Pop : ('ctx, 'a) index -> ('b * 'ctx, 'a) index

type ('ctx, 'a) expr =
  | Global_var : 'a Global.t -> ('ctx, 'a) expr
  | Local_var : ('ctx, 'a) index -> ('ctx, 'a) expr
  | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
  | Fun_abs : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
  | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr
  | Int_lit : int -> ('ctx, int) expr
  | String_lit : string -> ('ctx, string) expr

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
    | Global_var g -> Global.lookup g
    | Let (def, body) -> eval (eval env def :: env) body
    | Local_var x -> lookup x env
    | Fun_abs body -> fun x -> eval (x :: env) body
    | Fun_app (fn, arg) -> (eval env fn) (eval env arg)
    | Int_lit i -> i
    | String_lit s -> s

let () = begin

  let ( $ ) f x = Fun_app (f, x) in

  let zero : int Global.t = Global.define @@ 0 in
  let id (type a) : (a -> a) Global.t = Global.define @@ Fun.id in
  let const (type a b) : (a -> b -> a) Global.t = Global.define @@ Fun.const in

  print_string "Running tests ...";

  assert (eval [] (Fun_abs (Local_var Stop)) 1 = 1);
  assert (eval [] (Fun_abs (Fun_abs (Local_var (Pop Stop)))) "hello" 4 = "hello");
  assert (eval ["hello"] (Fun_abs (Fun_abs (Local_var (Pop Stop))) $ Local_var Stop) 4 = "hello");
  assert (eval [2; "hello"] (Let (Local_var (Pop Stop), Local_var Stop)) = "hello");
  assert (eval [] (Global_var id $ Global_var zero) = 0);
  assert (eval [] (Global_var const $ Int_lit 42 $ String_lit "hello") = 42);

  print_string " ok!\n";

end
