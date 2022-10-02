(** Test some properties of the compiler *)

module TreeLang = ArithCond.TreeLang
module StackLang = ArithCond.StackLang
module AnfLang = ArithCond.AnfLang


(** {1 Generators} *)

(** Term generation (probably ill-typed) *)

let gen_expr_untyped : TreeLang.expr QCheck.Gen.t =
  let open QCheck.Gen in
  sized @@ fix
    (fun self -> function
      | 0 -> oneof [map TreeLang.int big_nat; map TreeLang.bool bool]
      | n ->
          frequency [
            1, map TreeLang.int nat;
            1, map TreeLang.bool bool;
            2, map TreeLang.neg (self (n/2));
            3, map2 TreeLang.add (self (n/2)) (self (n/2));
            3, map2 TreeLang.sub (self (n/2)) (self (n/2));
            3, map2 TreeLang.mul (self (n/2)) (self (n/2));
            3, map2 TreeLang.div (self (n/2)) (self (n/2));
            3, map2 TreeLang.eq (self (n/2)) (self (n/2));
            4, map3 TreeLang.if_then_else (self (n/2)) (self (n/2)) (self (n/2));
          ]
      )


(** Term generation (well-typed) *)

let gen_ty : TreeLang.ty QCheck.Gen.t =
  QCheck.Gen.oneofl [
    TreeLang.TyInt;
    TreeLang.TyBool;
  ]

let gen_expr gen_ty : TreeLang.expr QCheck.Gen.t =
  let open QCheck.Gen in
  pair gen_ty nat >>= fix
    (fun self -> function
      | TreeLang.TyInt, 0 -> map TreeLang.int big_nat
      | TreeLang.TyBool, 0 -> map TreeLang.bool bool
      | TreeLang.TyInt, n ->
          frequency [
            1, map TreeLang.int nat;
            2, map TreeLang.neg (self (TreeLang.TyInt, n/2));
            3, map2 TreeLang.add (self (TreeLang.TyInt, n/2)) (self (TreeLang.TyInt, n/2));
            3, map2 TreeLang.sub (self (TreeLang.TyInt, n/2)) (self (TreeLang.TyInt, n/2));
            3, map2 TreeLang.mul (self (TreeLang.TyInt, n/2)) (self (TreeLang.TyInt, n/2));
            3, map2 TreeLang.div (self (TreeLang.TyInt, n/2)) (self (TreeLang.TyInt, n/2));
            4, map3 TreeLang.if_then_else
              (self (TreeLang.TyBool, n/2))
              (self (TreeLang.TyInt, n/2))
              (self (TreeLang.TyInt, n/2));
          ]
      | TreeLang.TyBool, n ->
          frequency [
            1, map TreeLang.bool bool;
            3, (let* ty = gen_ty in map2 TreeLang.eq (self (ty, n/2)) (self (ty, n/2)));
            4, map3 TreeLang.if_then_else
              (self (TreeLang.TyBool, n/2))
              (self (TreeLang.TyBool, n/2))
              (self (TreeLang.TyBool, n/2));
          ]
      )


(** {1 Shrinkers} *)

let rec shrink_expr =
  let shrink_bool b yield =
    if b then yield false else ()
  in
  QCheck.Iter.(function
  | TreeLang.Int i -> map TreeLang.int (QCheck.Shrink.int i)
  | TreeLang.Bool b -> map TreeLang.bool (shrink_bool b)
  | TreeLang.Neg e ->
      QCheck.Iter.of_list [e]
        <+> map TreeLang.neg (shrink_expr e)
  | TreeLang.Add (e1, e2) ->
      QCheck.Iter.of_list [e1; e2]
        <+> (let+ e1' = shrink_expr e1 in TreeLang.add e1' e2)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.add e1 e2')
  | TreeLang.Sub (e1, e2) ->
      QCheck.Iter.of_list [e1; e2]
        <+> (let+ e1' = shrink_expr e1 in TreeLang.sub e1' e2)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.sub e1 e2')
  | TreeLang.Mul (e1, e2) ->
      QCheck.Iter.of_list [e1; e2]
        <+> (let+ e1' = shrink_expr e1 in TreeLang.mul e1' e2)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.mul e1 e2')
  | TreeLang.Div (e1, e2) ->
      QCheck.Iter.of_list [e1; e2]
        <+> (let+ e1' = shrink_expr e1 in TreeLang.div e1' e2)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.div e1 e2')
  | TreeLang.Eq (e1, e2) ->
      QCheck.Iter.empty (* the subexpressions might not be booleans, so don't shrink to them *)
        <+> (let+ e1' = shrink_expr e1 in TreeLang.eq e1' e2)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.eq e1 e2')
  | TreeLang.IfThenElse (e1, e2, e3) ->
      (* shrink to either branch of the conditional, but not the boolean expression *)
      QCheck.Iter.of_list [e2; e3]
        <+> (let+ e1' = shrink_expr e1 in TreeLang.if_then_else e1' e2 e3)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.if_then_else e1 e2' e3)
        <+> (let+ e3' = shrink_expr e3 in TreeLang.if_then_else e1 e2 e3'))


(** {1 Arbitrary generators + shrinkers + printers} *)

let arbitrary_expr_untyped =
  QCheck.make gen_expr_untyped
    ~print:(Format.asprintf "%a" TreeLang.pp_expr)
    ~shrink:shrink_expr

let arbitrary_expr gen_ty =
  let gen = QCheck.Gen.(let* t = gen_ty in pair (gen_expr (pure t)) (pure t)) in
  let print (e, t) = Format.asprintf "@[@[@[%a@]@ :@]@ %a@]" TreeLang.pp_expr e TreeLang.pp_ty t in
  let shrink (e, t) = QCheck.Iter.(let+ e' = shrink_expr e in e', t) in
  QCheck.make gen ~print ~shrink


(** {1 Properties} *)

(* TODO: improve handling of divide-by-zero in evaluators? *)
let catch_div_by_zero f =
  try Ok (f ()) with Division_by_zero as e -> Error e

(** The compiler preserves the semantics of the arithmetic expressions *)
let compile_stack_correct =
  (* TODO: improve handling of divide-by-zero *)
  let eval_tree e = catch_div_by_zero (fun () -> TreeLang.Semantics.eval e) in
  let eval_stack e = catch_div_by_zero (fun () -> StackLang.Semantics.eval e) in
  let to_stack = Result.map (function
    | TreeLang.Semantics.Int n -> [StackLang.Semantics.Int n]
    | TreeLang.Semantics.Bool b -> [StackLang.Semantics.Bool b])
  in

  QCheck.Test.make ~count:1000
    ~name:"compile_stack_correct"
    (arbitrary_expr gen_ty)
    (fun (e, _) ->
      eval_stack (ArithCond.TreeToStack.translate e) = to_stack (eval_tree e))

(** Compilation preserves the semantics of the arithmetic expressions *)
let compile_anf_correct =
  let eval_tree e = catch_div_by_zero (fun () -> TreeLang.Semantics.eval e) in
  let eval_anf e = catch_div_by_zero (fun () -> AnfLang.Semantics.eval e) in
  let to_anf = Result.map (function
    | TreeLang.Semantics.Int n -> AnfLang.Semantics.Int n
    | TreeLang.Semantics.Bool b -> AnfLang.Semantics.Bool b)
  in

  QCheck.Test.make ~count:1000
    ~name:"compile_anf_correct"
    (arbitrary_expr gen_ty)
    (fun (e, _) ->
      eval_anf (ArithCond.TreeToAnf.translate e) = to_anf (eval_tree e))

(** All well-typed expressions should pass the type checker *)
let check_correct =
  QCheck.Test.make ~count:1000
    ~name:"check_correct"
    (arbitrary_expr gen_ty)
    (fun (e, t) -> TreeLang.Validation.check e t; true)

(* TODO: Test progress and preservation *)
(* TODO: Test errors *)

(** Pretty printed expression can always be parsed back into the same expression. *)
let pretty_correct =
  let pretty e = Format.asprintf "%a" TreeLang.pp_expr e in
  let parse source = TreeLang.(Parser.main Lexer.token (Lexing.from_string source)) in

  QCheck.Test.make ~count:1000
    ~name:"pretty_correct"
    arbitrary_expr_untyped
    (fun e -> parse (pretty e) = e)


(** {1 Entrypoint of test harness} *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest [
      compile_stack_correct;
      compile_anf_correct;
      check_correct;
      pretty_correct;
    ]
  in
  Alcotest.run "compile-arithcond" [
    "properties", suite;
  ]
