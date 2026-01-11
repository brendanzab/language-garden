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

  (* Value types *)

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
  type 'a comp = Comp

  (* Contexts *)

  type empty = Empty
  type ('a, 'ctx) extend = Extend

end


(* Syntax *)

type ('ctx, 'a) index =
  | Stop : (('a, 'ctx) T.extend, 'a) index
  | Pop : ('ctx, 'a1) index -> (('a2, 'ctx) T.extend, 'a1) index

type 'a prim =
  | Int_eq : (T.int, (T.int, T.bool T.comp) T.func) T.func prim
  | Int_add : (T.int, (T.int, T.int T.comp) T.func) T.func prim
  | Int_sub : (T.int, (T.int, T.int T.comp) T.func) T.func prim
  | Int_mul : (T.int, (T.int, T.int T.comp) T.func) T.func prim
  | Int_neg : (T.int, T.int T.comp) T.func prim
  | String_eq : (T.string, (T.string, T.bool T.comp) T.func) T.func prim
  | String_cat : (T.string, (T.string, T.string T.comp) T.func) T.func prim

type ('ctx, 'a) tm =
  | Var : ('ctx, 'a) index -> ('ctx, 'a) tm
  | Thunk_lit : ('ctx, 'b) comp_tm -> ('ctx, 'b T.thunk) tm
  | Pair_lit : ('ctx, 'a1) tm * ('ctx, 'a2) tm -> ('ctx, ('a1, 'a2) T.pair) tm
  | Either_left : ('ctx, 'a1) tm -> ('ctx, ('a1, 'a2) T.either) tm
  | Either_right : ('ctx, 'a2) tm -> ('ctx, ('a1, 'a2) T.either) tm
  | Unit_lit : ('ctx, T.unit) tm
  | Int_lit : int -> ('ctx, T.int) tm
  | Bool_lit : bool -> ('ctx, T.bool) tm
  | String_lit : string -> ('ctx, T.string) tm

and ('ctx, 'a) comp_tm =
  | Let : ('ctx, 'a) tm * (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Fix : (('b T.thunk, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Thunk_force : ('ctx, 'b T.thunk) tm -> ('ctx, 'b) comp_tm
  | Fun_lit : (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, ('a, 'b) T.func) comp_tm
  | Fun_app : ('ctx, ('a, 'b) T.func) comp_tm * ('ctx, 'a) tm -> ('ctx, 'b) comp_tm
  | Record_lit : ('ctx, 'b1) comp_tm * ('ctx, 'b2) comp_tm -> ('ctx, ('b1, 'b2) T.record) comp_tm
  | Record_left : ('ctx, ('b1, 'b2) T.record) comp_tm -> ('ctx, 'b1) comp_tm
  | Record_right : ('ctx, ('b1, 'b2) T.record) comp_tm -> ('ctx, 'b2) comp_tm
  | Pair_match :
      ('ctx, ('a1, 'a2) T.pair) tm
      * (('a2, ('a1, 'ctx) T.extend) T.extend, 'b) comp_tm
      -> ('ctx, 'b) comp_tm
  | Either_match :
      ('ctx, ('a1, 'a2) T.either) tm
      * (('a1, 'ctx) T.extend, 'b) comp_tm
      * (('a2, 'ctx) T.extend, 'b) comp_tm
      -> ('ctx, 'b) comp_tm
  | Bool_match : ('ctx, T.bool) tm * ('ctx, 'b) comp_tm * ('ctx, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Void_elim : ('ctx, T.void) tm -> ('ctx, 'b) comp_tm
  | Comp_bind : ('ctx, 'a T.comp) comp_tm * (('a, 'ctx) T.extend, 'b) comp_tm -> ('ctx, 'b) comp_tm
  | Comp_pure : ('ctx, 'a) tm -> ('ctx, 'a T.comp) comp_tm
  | Comp_print : ('ctx, T.string) tm -> ('ctx, T.unit T.comp) comp_tm
  | Comp_read : ('ctx, T.string T.comp) comp_tm
  | Comp_abort : ('ctx, T.void T.comp) comp_tm
  | Prim : 'a prim -> ('ctx, 'a) comp_tm


(* Semantic domain *)

type 'a vtm =
  | Thunk_lit : (unit -> 'a comp_vtm) -> 'a T.thunk vtm
  | Pair_lit : 'a1 vtm * 'a2 vtm -> ('a1, 'a2) T.pair vtm
  | Either_left : 'a1 vtm -> ('a1, 'a2) T.either vtm
  | Either_right : 'a2 vtm -> ('a1, 'a2) T.either vtm
  | Unit_lit : T.unit vtm
  | Int_lit : int -> T.int vtm
  | Bool_lit : bool -> T.bool vtm
  | String_lit : string -> T.string vtm

and 'a comp_vtm =
  | Fun_lit : ('a vtm -> 'b comp_vtm) -> ('a, 'b) T.func comp_vtm
  | Record_lit : 'b1 comp_vtm * 'b2 comp_vtm -> ('b1, 'b2) T.record comp_vtm
  | Comp_pure : 'a vtm -> 'a T.comp comp_vtm


(* Environments *)

type 'ctx env =
  | [] : T.empty env
  | ( :: ) : 'a vtm * 'ctx env -> ('a, 'ctx) T.extend env


(* Evaluation *)

exception Abort

type _ Effect.t +=
  | Print : string -> unit Effect.t
  | Read : string Effect.t

let rec eval_index : type ctx a. (ctx, a) index -> ctx env -> a vtm =
  fun x env ->
    match x, env with
    | Stop, v :: _ -> v
    | Pop x, _ :: env -> eval_index x env

let eval_prim : type a. a prim -> a comp_vtm =
  let int_fun f = Fun_lit (fun (Int_lit x) -> f x) in
  let string_fun f = Fun_lit (fun (String_lit x) -> f x) in

  fun prim ->
    match prim with
    | Int_eq -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Comp_pure (Bool_lit (i1 = i2))
    | Int_add -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Comp_pure (Int_lit (i1 + i2))
    | Int_sub -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Comp_pure (Int_lit (i1 - i2))
    | Int_mul -> int_fun @@ fun i1 -> int_fun @@ fun i2 -> Comp_pure (Int_lit (i1 * i2))
    | Int_neg -> int_fun @@ fun i -> Comp_pure (Int_lit (-i))
    | String_eq -> string_fun @@ fun s1 -> string_fun @@ fun s2 -> Comp_pure (Bool_lit (s1 = s2))
    | String_cat -> string_fun @@ fun s1 -> string_fun @@ fun s2 -> Comp_pure (String_lit (s1 ^ s2))

let rec eval_tm : type ctx a. ctx env -> (ctx, a) tm -> a vtm =
  fun env tm ->
    match tm with
    | Var x -> eval_index x env
    | Thunk_lit tm -> Thunk_lit (fun () -> eval_comp_tm env tm)
    | Pair_lit (left, right) -> Pair_lit (eval_tm env left, eval_tm env right)
    | Either_left left -> Either_left (eval_tm env left)
    | Either_right right -> Either_right (eval_tm env right)
    | Unit_lit -> Unit_lit
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | String_lit s -> String_lit s

and eval_comp_tm : type ctx a. ctx env -> (ctx, a) comp_tm -> a comp_vtm =
  fun env tm ->
    match tm with
    | Let (def, body) ->
        let def = eval_tm env def in
        eval_comp_tm (def :: env) body

    | Fix thunk ->
        eval_comp_tm (eval_tm env (Thunk_lit tm) :: env) thunk

    | Thunk_force thunk ->
        let Thunk_lit value = eval_tm env thunk in
        value ()

    | Fun_lit body ->
        Fun_lit (fun param -> eval_comp_tm (param :: env) body)

    | Fun_app (fn, arg) ->
        let Fun_lit fn = eval_comp_tm env fn in
        fn (eval_tm env arg)

    | Record_lit (left, right) ->
        Record_lit (eval_comp_tm env left, eval_comp_tm env right)

    | Record_left record ->
        let Record_lit (left, _) = eval_comp_tm env record in left

    | Record_right record ->
        let Record_lit (_, right) = eval_comp_tm env record in right

    | Pair_match (pair, body) ->
        let Pair_lit (left, right) = eval_tm env pair in
        eval_comp_tm (right :: left :: env) body

    | Either_match (either, left, right) ->
        begin match eval_tm env either with
        | Either_left tm -> eval_comp_tm (tm :: env) left
        | Either_right tm -> eval_comp_tm (tm :: env) right
        end

    | Bool_match (bool, on_true, on_false) ->
        begin match eval_tm env bool with
        | Bool_lit true -> eval_comp_tm env on_true
        | Bool_lit false -> eval_comp_tm env on_false
        end

    | Void_elim void ->
        begin match eval_tm env void with
        | _ -> .
        end

    | Comp_bind (def, body) ->
        let Comp_pure def = eval_comp_tm env def in
        eval_comp_tm (def :: env) body

    | Comp_pure tm ->
        Comp_pure (eval_tm env tm)

    | Comp_print str ->
        let String_lit str = eval_tm env str in
        Effect.perform (Print str);
        Comp_pure Unit_lit

    | Comp_read ->
        let str = Effect.perform Read in
        Comp_pure (String_lit str)

    | Comp_abort ->
        raise Abort

    | Prim prim ->
        eval_prim prim


(* Evaluation tests *)

let () = begin

  Printexc.record_backtrace true;

  let run_tests (type a) (prog : (string -> (unit -> unit) -> unit) -> unit) : a =
    let success_count = ref 0 in
    let error_count = ref 0 in

    let run_test (name : string) (prog : unit -> unit) : unit =
      Printf.printf "test %s ... " name;

      match prog () with
      | () ->
          Printf.printf "ok\n";
          incr success_count
      | exception e ->
          Printf.printf "error:\n\n";
          Printf.printf "  %s\n\n" (Printexc.to_string e);
          String.split_on_char '\n' (Printexc.get_backtrace()) |> List.iter begin fun line ->
            Printf.printf "  %s\n" line;
          end;
          incr error_count
    in

    Printf.printf "Running tests in %s:\n\n" __FILE__;
    prog run_test;
    Printf.printf "\n";

    if !error_count <= 0 then begin
      Printf.printf "Ran %i successful tests\n\n" !success_count;
      exit 0
    end else begin
      Printf.printf "Failed %i out of %i tests\n\n" !error_count (!success_count + !error_count);
      exit 1
    end
  in

  let ( $ ) f a = Fun_app (f, a) in

  begin run_tests @@ fun test ->

    let test_eval_comp_tm (type a)
        ?(expected : a comp_vtm option)
        ?(expected_io : [`In of string | `Out of string] list = [])
        (name : string)
        (tm : (T.empty, a) comp_tm) : unit =

      let expected_io = Queue.of_seq (List.to_seq expected_io) in

      test name @@ fun () ->
        match eval_comp_tm [] tm, expected with
        | vtm, Some expected -> assert (vtm = expected)
        | _, None -> failwith "expected abort"
        | effect (Print str), k ->
            begin match Queue.pop expected_io with
            | `Out output when output = str -> Effect.Deep.continue k ()
            | `Out _ -> Effect.Deep.discontinue k (Failure "mismatched output")
            | `In _ -> Effect.Deep.discontinue k (Failure "expected output")
            end
        | effect Read, k ->
            begin match Queue.pop expected_io with
            | `In input -> Effect.Deep.continue k input
            | `Out _ -> Effect.Deep.discontinue k (Failure "expected input")
            end
        | exception Abort ->
            begin match expected with
            | Some _ -> failwith "expected value"
            | None -> ()
            end
    in

    test_eval_comp_tm "apply id"
      (Fun_lit (Comp_pure (Var Stop)) $ Int_lit 42)
      ~expected:(Comp_pure (Int_lit 42));

    test_eval_comp_tm "apply neg"
      (Prim Int_neg $ Int_lit 42)
      ~expected:(Comp_pure (Int_lit (-42)));

    test_eval_comp_tm "define and apply id"
      (Let (Thunk_lit (Fun_lit (Comp_pure (Var Stop))),
        Thunk_force (Var Stop) $ Unit_lit))
      ~expected:(Comp_pure Unit_lit);

    test_eval_comp_tm "match pair left"
      (Pair_match (Pair_lit (String_lit "hello", Int_lit 42),
        Comp_pure (Var (Pop Stop))))
      ~expected:(Comp_pure (String_lit "hello"));

    test_eval_comp_tm  "match pair right"
      (Pair_match (Pair_lit (String_lit "hello", Int_lit 42),
        Comp_pure (Var Stop)))
      ~expected:(Comp_pure (Int_lit 42));

    test_eval_comp_tm "record left"
      (Record_left (Record_lit (Comp_pure (String_lit "hello"), Comp_pure (Int_lit 42))))
      ~expected:(Comp_pure (String_lit "hello"));

    test_eval_comp_tm "record right"
      (Record_right (Record_lit (Comp_pure (String_lit "hello"), Comp_pure (Int_lit 42))))
      ~expected:(Comp_pure (Int_lit 42));

    test_eval_comp_tm "match either left"
      (Either_match (Either_left (String_lit "hello"),
        Comp_pure (Var Stop),
        Void_elim (Var Stop)))
      ~expected:(Comp_pure (String_lit "hello"));

    test_eval_comp_tm "match eight right"
      (Either_match (Either_right Unit_lit,
        Comp_pure (Var Stop),
        Comp_pure (String_lit "goodbye")))
      ~expected:(Comp_pure (String_lit "goodbye"));

    test_eval_comp_tm "bind value and let 1"
      (Comp_bind (Comp_pure (Int_lit 42),
        Let (String_lit "hello",
          Comp_pure (Var (Pop Stop)))))
      ~expected:(Comp_pure (Int_lit 42));

    test_eval_comp_tm "bind value and let 2"
      (Comp_bind (Comp_pure (Int_lit 42),
        Let (String_lit "hello",
          Comp_pure (Var Stop))))
      ~expected:(Comp_pure (String_lit "hello"));

    test_eval_comp_tm "print"
      (Comp_print (String_lit "hello"))
      ~expected:(Comp_pure Unit_lit)
      ~expected_io:[`Out "hello"];

    test_eval_comp_tm "read"
      Comp_read
      ~expected:(Comp_pure (String_lit "howdy"))
      ~expected_io:[`In "howdy"];

    test_eval_comp_tm "abort"
      Comp_abort;

    test_eval_comp_tm "hello world"
      (Let (Thunk_lit Comp_read,
        Comp_bind (Thunk_force (Var Stop),
          Comp_bind (Thunk_force (Var (Pop Stop)),
            Prim String_cat $ Var (Pop Stop) $ Var Stop))))
      ~expected:(Comp_pure (String_lit "hello world!"))
      ~expected_io:[`In "hello "; `In "world!"];

  end;

end
