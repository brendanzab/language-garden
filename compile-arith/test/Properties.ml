(** Test some properties of the compiler *)

module TreeLang = Arith.TreeLang
module StackLang = Arith.StackLang


(** Arbitrary term generation *)

let term_gen = QCheck.Gen.(sized @@ fix
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

let arbitrary_term =
  let print_term term =
    Format.asprintf "%a" TreeLang.pp_term term
  in
  let rec shrink_term = QCheck.Iter.(function
    | TreeLang.Num i -> map TreeLang.num (QCheck.Shrink.int i)
    | TreeLang.Neg n ->
        QCheck.Iter.of_list [n]
          <+> map TreeLang.neg (shrink_term n)
    | TreeLang.Add (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_term n1 in TreeLang.add n1' n2)
          <+> (let+ n2' = shrink_term n2 in TreeLang.add n1 n2')
    | TreeLang.Sub (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_term n1 in TreeLang.sub n1' n2)
          <+> (let+ n2' = shrink_term n2 in TreeLang.sub n1 n2')
    | TreeLang.Mul (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_term n1 in TreeLang.mul n1' n2)
          <+> (let+ n2' = shrink_term n2 in TreeLang.mul n1 n2')
    | TreeLang.Div (n1, n2) ->
        QCheck.Iter.of_list [n1; n2]
          <+> (let+ n1' = shrink_term n1 in TreeLang.div n1' n2)
          <+> (let+ n2' = shrink_term n2 in TreeLang.div n1 n2'))
  in
  QCheck.make term_gen
    ~print:print_term
    ~shrink:shrink_term


(** Properties *)

(** The compiler preserves the semantics of the arithmetic expressions *)
let compile_correct =
  (* TODO: improve handling of divide-by-zero *)
  let catch_div_by_zero f = try Ok (f ()) with Division_by_zero as e -> Error e in
  let eval_tree term = catch_div_by_zero (fun () -> TreeLang.Semantics.eval term) in
  let eval_stack term = catch_div_by_zero (fun () -> StackLang.Semantics.eval term) in
  let to_stack = Result.map (fun value -> [value]) in

  QCheck.Test.make ~count:1000
    ~name:"compile_correct"
    arbitrary_term
    (fun term ->
      eval_stack (Arith.TreeToStack.translate term) = to_stack (eval_tree term))

(** Pretty printed term can always be parsed back into the same term. *)
let pretty_correct =
  let pretty term = Format.asprintf "%a" TreeLang.pp_term term in
  let parse source = TreeLang.(Parser.main Lexer.token (Lexing.from_string source)) in

  QCheck.Test.make ~count:1000
    ~name:"pretty_correct"
    arbitrary_term
    (fun term -> parse (pretty term) = term)


(** Entrypoint for the tests *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [compile_correct; pretty_correct]
  in
  Alcotest.run "compile-arith" [
    "properties", suite
  ]
