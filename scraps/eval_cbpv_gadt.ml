(** A well-typed evaluator for a lambda calculus in the style of call-by-push-value.

    Extends [eval_stlc_gadt_values_hoas].

    Resources:

    - Paul Blain Levy, {{: https://web.archive.org/web/20241107133020/https://www.cs.bham.ac.uk/~pbl/cbpv.html}
      Call-By-Push-Value FAQ}
    - Paul Blain Levy, {{: https://pblevy.github.io/papers/thesisqmwphd.pdf}
      Call-By-Push-Value}, PhD Thesis
    - Andrej Bauer, {{: https://github.com/andrejbauer/plzoo/tree/master/src/levy} plzoo/levy}
    - Andras Kovacs, {{: https://www.reddit.com/r/ProgrammingLanguages/comments/1bam7bd/im_betting_on_callbypushvalue/ku4yel3}
      Comment on “I'm betting on Call-by-Push-Value”}
*)

[@@@warning "-unused-constructor"]


(** Phantom types *)
module T = struct

  (* Data types *)

  type 'b thunk = Thunk
  type ('a1, 'a2) pair = Pair
  type ('a1, 'a2) either = Either
  type unit = Unit
  type void = Void
  type int = Int
  type bool = Bool
  type string = String

  (* Computation types *)

  type ('a, 'b) func = Func
  type ('b1, 'b2) record = Record
  type 'a eff = Eff

  (* Contexts *)

  type empty = Empty
  type ('a, 'ctx) extend = Extend

end


(* Syntax *)

type ('ctx, 'a) index =
  | Stop : (('a, 'ctx) T.extend, 'a) index
  | Pop : ('ctx, 'a1) index -> (('a2, 'ctx) T.extend, 'a1) index

type 'a prim =
  | Int_eq : (T.int, (T.int, T.bool T.eff) T.func) T.func prim
  | Int_add : (T.int, (T.int, T.int T.eff) T.func) T.func prim
  | Int_sub : (T.int, (T.int, T.int T.eff) T.func) T.func prim
  | Int_mul : (T.int, (T.int, T.int T.eff) T.func) T.func prim
  | Int_neg : (T.int, T.int T.eff) T.func prim
  | String_eq : (T.string, (T.string, T.bool T.eff) T.func) T.func prim
  | String_cat : (T.string, (T.string, T.string T.eff) T.func) T.func prim

type ('ctx, 'a) data_tm =
  | Var : ('ctx, 'a) index -> ('ctx, 'a) data_tm
  | Thunk_lit : ('ctx, 'b) comp_tm -> ('ctx, 'b T.thunk) data_tm
  | Pair_lit : ('ctx, 'a1) data_tm * ('ctx, 'a2) data_tm -> ('ctx, ('a1, 'a2) T.pair) data_tm
  | Either_left : ('ctx, 'a1) data_tm -> ('ctx, ('a1, 'a2) T.either) data_tm
  | Either_right : ('ctx, 'a2) data_tm -> ('ctx, ('a1, 'a2) T.either) data_tm
  | Unit_lit : ('ctx, T.unit) data_tm
  | Int_lit : int -> ('ctx, T.int) data_tm
  | Bool_lit : bool -> ('ctx, T.bool) data_tm
  | String_lit : string -> ('ctx, T.string) data_tm

and ('ctx, 'a) comp_tm =
  | Let : ('ctx, 'a) data_tm * (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Fix : (('b T.thunk, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Thunk_force : ('ctx, 'b T.thunk) data_tm -> ('ctx, 'b) comp_tm
  | Fun_lit : (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, ('a, 'b) T.func) comp_tm
  | Fun_app : ('ctx, ('a, 'b) T.func) comp_tm * ('ctx, 'a) data_tm -> ('ctx, 'b) comp_tm
  | Record_lit : ('ctx, 'b1) comp_tm * ('ctx, 'b2) comp_tm -> ('ctx, ('b1, 'b2) T.record) comp_tm
  | Record_left : ('ctx, ('b1, 'b2) T.record) comp_tm -> ('ctx, 'b1) comp_tm
  | Record_right : ('ctx, ('b1, 'b2) T.record) comp_tm -> ('ctx, 'b2) comp_tm
  | Pair_match :
      ('ctx, ('a1, 'a2) T.pair) data_tm
      * (('a2, ('a1, 'ctx) T.extend) T.extend, 'b) comp_tm
      -> ('ctx, 'b) comp_tm
  | Either_match :
      ('ctx, ('a1, 'a2) T.either) data_tm
      * (('a1, 'ctx) T.extend, 'b) comp_tm
      * (('a2, 'ctx) T.extend, 'b) comp_tm
      -> ('ctx, 'b) comp_tm
  | Bool_match : ('ctx, T.bool) data_tm * ('ctx, 'b) comp_tm * ('ctx, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Void_elim : ('ctx, T.void) data_tm -> ('ctx, 'b) comp_tm
  | Eff_bind : ('ctx, 'a T.eff) comp_tm * (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Eff_pure : ('ctx, 'a) data_tm -> ('ctx, 'a T.eff) comp_tm
  | Eff_print : ('ctx, T.string) data_tm -> ('ctx, T.unit T.eff) comp_tm
  | Eff_read : ('ctx, T.string T.eff) comp_tm
  | Eff_abort : ('ctx, T.void T.eff) comp_tm
  | Prim : 'a prim -> ('ctx, 'a) comp_tm


(* Semantic domain *)

type 'a data_vtm =
  | Thunk_lit : (unit -> 'a comp_vtm) -> 'a T.thunk data_vtm
  | Pair_lit : 'a1 data_vtm * 'a2 data_vtm -> ('a1, 'a2) T.pair data_vtm
  | Either_left : 'a1 data_vtm -> ('a1, 'a2) T.either data_vtm
  | Either_right : 'a2 data_vtm -> ('a1, 'a2) T.either data_vtm
  | Unit_lit : T.unit data_vtm
  | Int_lit : int -> T.int data_vtm
  | Bool_lit : bool -> T.bool data_vtm
  | String_lit : string -> T.string data_vtm

and 'a comp_vtm =
  | Fun_lit : ('a data_vtm -> 'b comp_vtm) -> ('a, 'b) T.func comp_vtm
  | Record_lit : 'b1 comp_vtm * 'b2 comp_vtm -> ('b1, 'b2) T.record comp_vtm
  | Eff_pure : 'a data_vtm -> 'a T.eff comp_vtm


(* Environments *)

type 'ctx env =
  | [] : T.empty env
  | ( :: ) : 'a data_vtm * 'ctx env -> ('a, 'ctx) T.extend env


(* Evaluation *)

exception Abort

type _ Effect.t +=
  | Print : string -> unit Effect.t
  | Read : string Effect.t

let rec eval_index : type ctx a. (ctx, a) index -> ctx env -> a data_vtm =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> eval_index x env

let eval_prim : type a. a prim -> a comp_vtm =
  let int_fun f = Fun_lit (fun (Int_lit x) -> f x) in
  let string_fun f = Fun_lit (fun (String_lit x) -> f x) in

  fun prim ->
    match prim with
    | Int_eq -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Eff_pure (Bool_lit (i1 = i2))
    | Int_add -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Eff_pure (Int_lit (i1 + i2))
    | Int_sub -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Eff_pure (Int_lit (i1 - i2))
    | Int_mul -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Eff_pure (Int_lit (i1 * i2))
    | Int_neg -> int_fun @@ fun i -> Eff_pure (Int_lit (-i))
    | String_eq -> string_fun @@ fun s1 -> string_fun @@ fun s2 -> Eff_pure (Bool_lit (s1 = s2))
    | String_cat -> string_fun @@ fun s1 -> string_fun @@ fun s2 -> Eff_pure (String_lit (s1 ^ s2))

let rec eval_data : type ctx a. ctx env -> (ctx, a) data_tm -> a data_vtm =
  fun env tm ->
    match tm with
    | Var x -> eval_index x env
    | Thunk_lit tm -> Thunk_lit (fun () -> eval_comp env tm)
    | Pair_lit (left, right) -> Pair_lit (eval_data env left, eval_data env right)
    | Either_left left -> Either_left (eval_data env left)
    | Either_right right -> Either_right (eval_data env right)
    | Unit_lit -> Unit_lit
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | String_lit s -> String_lit s

and eval_comp : type ctx a. ctx env -> (ctx, a) comp_tm -> a comp_vtm =
  fun env tm ->
    match tm with
    | Let (def, body) ->
        let def = eval_data env def in
        eval_comp (def :: env) body

    | Fix thunk ->
        eval_comp (eval_data env (Thunk_lit tm) :: env) thunk

    | Thunk_force thunk ->
        let Thunk_lit value = eval_data env thunk in
        value ()

    | Fun_lit body ->
        Fun_lit (fun param -> eval_comp (param :: env) body)

    | Fun_app (fn, arg) ->
        let Fun_lit fn = eval_comp env fn in
        fn (eval_data env arg)

    | Record_lit (left, right) ->
        Record_lit (eval_comp env left, eval_comp env right)

    | Record_left record ->
        let Record_lit (left, _) = eval_comp env record in left

    | Record_right record ->
        let Record_lit (_, right) = eval_comp env record in right

    | Pair_match (pair, body) ->
        let Pair_lit (left, right) = eval_data env pair in
        eval_comp (right :: left :: env) body

    | Either_match (either, left, right) ->
        begin match eval_data env either with
        | Either_left tm -> eval_comp (tm :: env) left
        | Either_right tm -> eval_comp (tm :: env) right
        end

    | Bool_match (bool, on_true, on_false) ->
        begin match eval_data env bool with
        | Bool_lit true -> eval_comp env on_true
        | Bool_lit false -> eval_comp env on_false
        end

    | Void_elim void ->
        begin match eval_data env void with
        | _ -> .
        end

    | Eff_bind (def, body) ->
        let Eff_pure def = eval_comp env def in
        eval_comp (def :: env) body

    | Eff_pure tm ->
        Eff_pure (eval_data env tm)

    | Eff_print str ->
        let String_lit str = eval_data env str in
        Effect.perform (Print str);
        Eff_pure Unit_lit

    | Eff_read ->
        let str = Effect.perform Read in
        Eff_pure (String_lit str)

    | Eff_abort ->
        raise Abort

    | Prim prim ->
        eval_prim prim


(* Evaluation tests *)

let () = begin

  let ( $ ) f a = Fun_app (f, a) in

  print_string "Running tests ...";

  assert (eval_comp []
    (Fun_lit (Eff_pure (Var Stop)) $ Int_lit 42)
      = Eff_pure (Int_lit 42));

  assert (eval_comp []
    (Prim Int_neg $ Int_lit 42)
      = Eff_pure (Int_lit (-42)));

  assert (eval_comp []
    (Let (Thunk_lit (Fun_lit (Eff_pure (Var Stop))),
      Thunk_force (Var Stop) $ Unit_lit))
      = Eff_pure Unit_lit);

  assert (eval_comp []
    (Pair_match (Pair_lit (String_lit "hello", Int_lit 42),
      Eff_pure (Var (Pop Stop))))
      = Eff_pure (String_lit "hello"));

  assert (eval_comp []
    (Pair_match (Pair_lit (String_lit "hello", Int_lit 42),
      Eff_pure (Var Stop)))
      = Eff_pure (Int_lit 42));

  assert (eval_comp []
    (Record_left (Record_lit (Eff_pure (String_lit "hello"), Eff_pure (Int_lit 42))))
      = Eff_pure (String_lit "hello"));

  assert (eval_comp []
    (Record_right (Record_lit (Eff_pure (String_lit "hello"), Eff_pure (Int_lit 42))))
      = Eff_pure (Int_lit 42));

  assert (eval_comp []
    (Either_match (Either_left (String_lit "hello"),
      Eff_pure (Var Stop),
      Void_elim (Var Stop)))
      = Eff_pure (String_lit "hello"));

  assert (eval_comp []
    (Either_match (Either_right Unit_lit,
      Eff_pure (Var Stop),
      Eff_pure (String_lit "goodbye")))
      = Eff_pure (String_lit "goodbye"));

  assert (eval_comp []
    (Eff_bind (Eff_pure (Int_lit 42),
      Let (String_lit "hello",
        Eff_pure (Var (Pop Stop)))))
      = Eff_pure (Int_lit 42));

  assert (eval_comp []
    (Eff_bind (Eff_pure (Int_lit 42),
      Let (String_lit "hello",
        Eff_pure (Var Stop))))
      = Eff_pure (String_lit "hello"));

  begin match eval_comp [] (Eff_print (String_lit "hello")) with
  | result -> assert (result = Eff_pure Unit_lit)
  | effect (Print str), k ->
      assert (str = "hello");
      Effect.Deep.continue k ()
  end;

  begin match eval_comp [] Eff_read with
  | result -> assert (result = Eff_pure (String_lit "howdy"))
  | effect Read, k ->
      Effect.Deep.continue k "howdy"
  end;

  begin match eval_comp [] Eff_abort with
  | exception Abort -> ()
  | _ -> failwith "expected abort"
  end;

  begin
    let lines = Queue.create () in
    lines |> Queue.push "hello ";
    lines |> Queue.push "world!";

    match
      eval_comp []
        (Let (Thunk_lit Eff_read,
          Eff_bind (Thunk_force (Var Stop),
            Eff_bind (Thunk_force (Var (Pop Stop)),
              Prim String_cat $ Var (Pop Stop) $ Var Stop))))
    with
    | result -> assert (result = Eff_pure (String_lit "hello world!"))
    | effect Read, k ->
        Effect.Deep.continue k (Queue.pop lines)
  end;

  print_string " ok!\n";

end
