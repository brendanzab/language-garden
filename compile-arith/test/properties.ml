(** Test some properties of the compiler *)

module Tree_lang = Arith.Tree_lang
module Stack_lang = Arith.Stack_lang
module Anf_lang = Arith.Anf_lang


(** Arbitrary expr generation *)

let expr_gen = QCheck.Gen.(sized @@ fix
  (fun self n -> match n with
    | 0 -> map Tree_lang.int nat
    | n ->
      oneof_weighted [
        1, map Tree_lang.int nat;
        2, map Tree_lang.neg (self (n/2));
        3, map2 Tree_lang.add (self (n/2)) (self (n/2));
        3, map2 Tree_lang.sub (self (n/2)) (self (n/2));
        3, map2 Tree_lang.mul (self (n/2)) (self (n/2));
        3, map2 Tree_lang.div (self (n/2)) (self (n/2));
      ]
    ))

let arbitrary_expr =
  let print_expr e =
    Format.asprintf "%t" (Tree_lang.pp_expr e)
  in
  let rec shrink_expr = QCheck.Iter.(function
    | Tree_lang.Int i -> map Tree_lang.int (QCheck.Shrink.int i)
    | Tree_lang.Neg e ->
        QCheck.Iter.of_list [e]
          <+> map Tree_lang.neg (shrink_expr e)
    | Tree_lang.Add (e1, e2) ->
        QCheck.Iter.of_list [e1; e2]
          <+> (let+ e1' = shrink_expr e1 in Tree_lang.add e1' e2)
          <+> (let+ e2' = shrink_expr e2 in Tree_lang.add e1 e2')
    | Tree_lang.Sub (e1, e2) ->
        QCheck.Iter.of_list [e1; e2]
          <+> (let+ e1' = shrink_expr e1 in Tree_lang.sub e1' e2)
          <+> (let+ e2' = shrink_expr e2 in Tree_lang.sub e1 e2')
    | Tree_lang.Mul (e1, e2) ->
        QCheck.Iter.of_list [e1; e2]
          <+> (let+ e1' = shrink_expr e1 in Tree_lang.mul e1' e2)
          <+> (let+ e2' = shrink_expr e2 in Tree_lang.mul e1 e2')
    | Tree_lang.Div (e1, e2) ->
        QCheck.Iter.of_list [e1; e2]
          <+> (let+ e1' = shrink_expr e1 in Tree_lang.div e1' e2)
          <+> (let+ e2' = shrink_expr e2 in Tree_lang.div e1 e2'))
  in
  QCheck.make expr_gen
    ~print:print_expr
    ~shrink:shrink_expr


(** Properties *)

(* TODO: improve handling of divide-by-zero in evaluators? *)
let catch_div_by_zero f =
  try Ok (f ()) with Division_by_zero as e -> Error e

(** Compilation preserves the semantics of the arithmetic expressions *)
let compile_stack_correct =
  let eval_tree e = catch_div_by_zero Tree_lang.Semantics.(fun () -> eval e) in
  let eval_stack e = catch_div_by_zero Stack_lang.Semantics.(fun () -> eval (e, [])) in
  let to_stack = Result.map (fun value -> [value]) in

  QCheck.Test.make ~count:1000
    ~name:"compile_stack_correct"
    arbitrary_expr
    (fun e ->
      eval_stack (Arith.Tree_to_stack.translate e) = to_stack (eval_tree e))

(** Compilation preserves the semantics of the arithmetic expressions *)
let compile_anf_correct =
  let eval_tree e = catch_div_by_zero Tree_lang.Semantics.(fun () -> eval e) in
  let eval_anf e = catch_div_by_zero Anf_lang.Semantics.(fun () -> eval Env.empty e) in

  QCheck.Test.make ~count:1000
    ~name:"compile_anf_correct"
    arbitrary_expr
    (fun e ->
      eval_anf (Arith.Tree_to_anf.translate e) = eval_tree e)

(** Pretty printed expr can always be parsed back into the same expr. *)
let pretty_correct =
  let pretty e = Format.asprintf "%t" (Tree_lang.pp_expr e) in
  let parse source =
    Sedlexing.Utf8.from_string source
    |> Sedlexing.with_tokenizer Tree_lang.Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Tree_lang.Parser.main
  in

  QCheck.Test.make ~count:1000
    ~name:"pretty_correct"
    arbitrary_expr
    (fun e -> parse (pretty e) = e)


(** Entrypoint for the tests *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest [
      compile_stack_correct;
      compile_anf_correct;
      pretty_correct;
    ]
  in
  Alcotest.run "compile-arith" [
    "properties", suite
  ]
