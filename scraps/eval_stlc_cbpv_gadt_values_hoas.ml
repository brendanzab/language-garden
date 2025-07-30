(** A well-typed evaluator for a lambda calculus in the style of call-by-push-value.

    Extends [eval_stlc_gadt_values_hoas].
*)


(** Phantom types *)
module T = struct

  [@@@warning "-unused-constructor"]

  (* Data types *)

  type 'b thunk = Thunk
  type ('a1, 'a2) pair = Pair
  type ('a1, 'a2) either = Either
  type unit = Unit
  type int = Int
  type string = String

  (* Computation types *)

  type ('a, 'b) func = Func
  type 'a comp = Comp

  (* Contexts *)

  type empty = Empty
  type ('a, 'ctx) extend = Extend

end


(* Syntax *)

type ('ctx, 'a) index =
  | Stop : (('a, 'ctx) T.extend, 'a) index
  | Pop : ('ctx, 'a1) index -> (('a2, 'ctx) T.extend, 'a1) index

type ('ctx, 'a) data_tm =
  | Var : ('ctx, 'a) index -> ('ctx, 'a) data_tm
  | Thunk_lit : ('ctx, 'b) comp_tm -> ('ctx, 'b T.thunk) data_tm
  | Pair_lit : ('ctx, 'a1) data_tm * ('ctx, 'a2) data_tm -> ('ctx, ('a1, 'a2) T.pair) data_tm
  | Either_left : ('ctx, 'a1) data_tm -> ('ctx, ('a1, 'a2) T.either) data_tm
  | Either_right : ('ctx, 'a2) data_tm -> ('ctx, ('a1, 'a2) T.either) data_tm
  | Unit_lit : ('ctx, T.unit) data_tm
  | Int_lit : int -> ('ctx, T.int) data_tm
  | String_lit : string -> ('ctx, T.string) data_tm

and ('ctx, 'a) comp_tm =
  | Let : ('ctx, 'a) data_tm * (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Thunk_force : ('ctx, 'b T.thunk) data_tm -> ('ctx, 'b) comp_tm
  | Fun_lit : (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, ('a, 'b) T.func) comp_tm
  | Fun_app : ('ctx, ('a, 'b) T.func) comp_tm * ('ctx, 'a) data_tm -> ('ctx, 'b) comp_tm
  | Pair_elim :
      ('ctx, ('a1, 'a2) T.pair) data_tm
      * (('a2, ('a1, 'ctx) T.extend) T.extend, 'b) comp_tm
      -> ('ctx, 'b) comp_tm
  | Either_elim :
      ('ctx, ('a1, 'a2) T.either) data_tm
      * (('a1, 'ctx) T.extend, 'b) comp_tm
      * (('a2, 'ctx) T.extend, 'b) comp_tm
      -> ('ctx, 'b) comp_tm
  | Comp_bind : ('ctx, 'a T.comp) comp_tm * (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Comp_pure : ('ctx, 'a) data_tm -> ('ctx, 'a T.comp) comp_tm


(* Semantic domain *)

type 'a data_vtm =
  | Thunk_lit : 'a comp_vtm Lazy.t -> 'a T.thunk data_vtm
  | Pair_lit : 'a1 data_vtm * 'a2 data_vtm -> ('a1, 'a2) T.pair data_vtm
  | Either_left : 'a1 data_vtm -> ('a1, 'a2) T.either data_vtm
  | Either_right : 'a2 data_vtm -> ('a1, 'a2) T.either data_vtm
  | Unit_lit : T.unit data_vtm
  | Int_lit : int -> T.int data_vtm
  | String_lit : string -> T.string data_vtm

and 'a comp_vtm =
  | Fun_lit : ('a data_vtm -> 'b comp_vtm) -> ('a, 'b) T.func comp_vtm
  | Comp_pure : 'a data_vtm -> 'a T.comp comp_vtm


(* Environments *)

type 'ctx env =
  | [] : T.empty env
  | ( :: ) : 'a data_vtm * 'ctx env -> ('a, 'ctx) T.extend env

let rec lookup : type ctx a. (ctx, a) index -> ctx env -> a data_vtm =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> lookup x env


(* Evaluation *)

let rec eval_data : type ctx a. ctx env -> (ctx, a) data_tm -> a data_vtm =
  fun env tm ->
    match tm with
    | Var x -> lookup x env
    | Thunk_lit tm -> Thunk_lit (lazy (eval_comp env tm))
    | Pair_lit (fst, snd) -> Pair_lit (eval_data env fst, eval_data env snd)
    | Either_left left -> Either_left (eval_data env left)
    | Either_right right -> Either_right (eval_data env right)
    | Unit_lit -> Unit_lit
    | Int_lit i -> Int_lit i
    | String_lit s -> String_lit s

and eval_comp : type ctx a. ctx env -> (ctx, a) comp_tm -> a comp_vtm =
  fun env tm ->
    match tm with
    | Let (def, body) -> let def = eval_data env def in eval_comp (def :: env) body
    | Thunk_force tm -> let Thunk_lit value = eval_data env tm in Lazy.force value
    | Fun_lit body -> Fun_lit (fun x -> eval_comp (x :: env) body)
    | Fun_app (fn, arg) -> let Fun_lit fn = eval_comp env fn in fn (eval_data env arg)
    | Pair_elim (pair, body) ->
        begin match eval_data env pair with
        | Pair_lit (tm1, tm2) -> eval_comp (tm2 :: tm1 :: env) body
        end
    | Either_elim (either, left, right) ->
        begin match eval_data env either with
        | Either_left tm -> eval_comp (tm :: env) left
        | Either_right tm -> eval_comp (tm :: env) right
        end
    | Comp_bind (def, body) -> let Comp_pure def = eval_comp env def in eval_comp (def :: env) body
    | Comp_pure tm -> Comp_pure (eval_data env tm)



let () = begin

  print_string "Running tests ...";

  assert (eval_comp []
    (Fun_app (Fun_lit (Comp_pure (Var Stop)), Int_lit 42))
      = Comp_pure (Int_lit 42));

  assert (eval_comp []
    (Let (Thunk_lit (Fun_lit (Comp_pure (Var Stop))),
      (Fun_app (Thunk_force (Var Stop), Unit_lit))))
      = Comp_pure Unit_lit);

  assert (eval_comp []
    (Pair_elim (Pair_lit (String_lit "hello", Int_lit 42),
      Comp_pure (Var (Pop Stop))))
      = Comp_pure (String_lit "hello"));

  assert (eval_comp []
    (Pair_elim (Pair_lit (String_lit "hello", Int_lit 42),
      Comp_pure (Var Stop)))
      = Comp_pure (Int_lit 42));

  assert (eval_comp []
    (Either_elim (Either_left (String_lit "hello"),
      Comp_pure (Var Stop),
      Comp_pure (String_lit "goodbye")))
      = Comp_pure (String_lit "hello"));

  assert (eval_comp []
    (Either_elim (Either_right Unit_lit,
      Comp_pure (Var Stop),
      Comp_pure (String_lit "goodbye")))
      = Comp_pure (String_lit "goodbye"));

  assert (eval_comp []
    (Comp_bind (Comp_pure (Int_lit 42),
      Let (String_lit "hello",
        Comp_pure (Var (Pop Stop)))))
      = Comp_pure (Int_lit 42));

  assert (eval_comp []
    (Comp_bind (Comp_pure (Int_lit 42),
      Let (String_lit "hello",
        Comp_pure (Var Stop))))
      = Comp_pure (String_lit "hello"));

  print_string " ok!\n";

end
