(** Test some properties of the compiler *)

module TreeLang = Arith.TreeLang
module StackLang = Arith.StackLang


(** Arbitrary expr generation *)

let expr_gen = QCheck.Gen.(sized @@ fix
  (fun self n -> match n with
    | 0 -> map TreeLang.num nat
    | n ->
      frequency [
        1, map TreeLang.num nat;
        2, map TreeLang.neg (self (n/2));
        3, map2 TreeLang.add (self (n/2)) (self (n/2));
        3, map2 TreeLang.sub (self (n/2)) (self (n/2));
        3, map2 TreeLang.mul (self (n/2)) (self (n/2));
        3, map2 TreeLang.div (self (n/2)) (self (n/2));
      ]
    ))

let arbitrary_expr =
  let print_expr expr =
    Format.asprintf "%a" TreeLang.pp_expr expr
  in
  let rec shrink_expr = QCheck.Iter.(function
    | TreeLang.Num i -> map TreeLang.num (QCheck.Shrink.int i)
    | TreeLang.Neg n ->
        QCheck.Iter.of_list [n]
          <+> map TreeLang.neg (shrink_expr n)
    | TreeLang.Add (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_expr n1 in TreeLang.add n1' n2)
          <+> (let+ n2' = shrink_expr n2 in TreeLang.add n1 n2')
    | TreeLang.Sub (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_expr n1 in TreeLang.sub n1' n2)
          <+> (let+ n2' = shrink_expr n2 in TreeLang.sub n1 n2')
    | TreeLang.Mul (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_expr n1 in TreeLang.mul n1' n2)
          <+> (let+ n2' = shrink_expr n2 in TreeLang.mul n1 n2')
    | TreeLang.Div (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_expr n1 in TreeLang.div n1' n2)
          <+> (let+ n2' = shrink_expr n2 in TreeLang.div n1 n2'))
  in
  QCheck.make expr_gen
    ~print:print_expr
    ~shrink:shrink_expr


(** Properties *)

(** The compiler preserves the semantics of the arithmetic expressions *)
let compile_correct =
  (* TODO: improve handling of divide-by-zero *)
  let catch_div_by_zero f = try Ok (f ()) with Division_by_zero as e -> Error e in
  let eval_tree expr = catch_div_by_zero (fun () -> TreeLang.Semantics.eval expr) in
  let eval_stack expr = catch_div_by_zero (fun () -> StackLang.Semantics.eval expr) in
  let to_stack = Result.map (fun value -> [value]) in

  QCheck.Test.make ~count:1000
    ~name:"compile_correct"
    arbitrary_expr
    (fun expr ->
      eval_stack (Arith.TreeToStack.translate expr) = to_stack (eval_tree expr))

(** Pretty printed expr can always be parsed back into the same expr. *)
let pretty_correct =
  let pretty expr = Format.asprintf "%a" TreeLang.pp_expr expr in
  let parse source = TreeLang.(Parser.main Lexer.token (Lexing.from_string source)) in

  QCheck.Test.make ~count:1000
    ~name:"pretty_correct"
    arbitrary_expr
    (fun expr -> parse (pretty expr) = expr)


(** Entrypoint for the tests *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [compile_correct; pretty_correct]
  in
  Alcotest.run "compile-arith" [
    "properties", suite
  ]
