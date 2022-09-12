(** Test some properties of the compiler *)

module TreeLang = ArithCond.TreeLang
module StackLang = ArithCond.StackLang


(** {1 Generators} *)

(** Term generation (probably ill-typed) *)

let gen_expr =
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

type ty = [`Int | `Bool]

let gen_ty : ty QCheck.Gen.t =
  QCheck.Gen.oneofl [`Int; `Bool]

let gen_expr_well_typed =
  let open QCheck.Gen in
  pair gen_ty nat >>= fix
    (fun self -> function
      | `Int, 0 -> map TreeLang.int big_nat
      | `Bool, 0 -> map TreeLang.bool bool
      | `Int, n ->
          frequency [
            1, map TreeLang.int nat;
            2, map TreeLang.neg (self (`Int, n/2));
            3, map2 TreeLang.add (self (`Int, n/2)) (self (`Int, n/2));
            3, map2 TreeLang.sub (self (`Int, n/2)) (self (`Int, n/2));
            3, map2 TreeLang.mul (self (`Int, n/2)) (self (`Int, n/2));
            3, map2 TreeLang.div (self (`Int, n/2)) (self (`Int, n/2));
            4, map3 TreeLang.if_then_else (self (`Bool, n/2)) (self (`Int, n/2)) (self (`Int, n/2));
          ]
      | `Bool, n ->
          frequency [
            1, map TreeLang.bool bool;
            3, (let* ty = gen_ty in map2 TreeLang.eq (self (ty, n/2)) (self (ty, n/2)));
            4, map3 TreeLang.if_then_else (self (`Bool, n/2)) (self (`Bool, n/2)) (self (`Bool, n/2));
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

let arbitrary_expr =
  QCheck.make gen_expr
    ~print:(Format.asprintf "%a" TreeLang.pp_expr)
    ~shrink:shrink_expr

let arbitrary_expr_well_typed =
  QCheck.make gen_expr_well_typed
    ~print:(Format.asprintf "%a" TreeLang.pp_expr)
    ~shrink:shrink_expr


(** {1 Properties} *)

(** The compiler preserves the semantics of the arithmetic expressions *)
let compile_correct =
  (* TODO: improve handling of divide-by-zero *)
  let catch_div_by_zero f = try Ok (f ()) with Division_by_zero as e -> Error e in
  let eval_tree expr = catch_div_by_zero (fun () -> TreeLang.Semantics.eval expr) in
  let eval_stack expr = catch_div_by_zero (fun () -> StackLang.Semantics.eval expr) in
  let to_stack = Result.map (function
    | TreeLang.Semantics.Int n -> [StackLang.Semantics.Int n]
    | TreeLang.Semantics.Bool b -> [StackLang.Semantics.Bool b])
  in

  QCheck.Test.make ~count:1000
    ~name:"compile_correct"
    arbitrary_expr_well_typed
    (fun expr ->
      eval_stack (ArithCond.TreeToStack.translate expr) = to_stack (eval_tree expr))

(** Pretty printed expr can always be parsed back into the same expr. *)
let pretty_correct =
  let pretty expr = Format.asprintf "%a" TreeLang.pp_expr expr in
  let parse source = TreeLang.(Parser.main Lexer.token (Lexing.from_string source)) in

  QCheck.Test.make ~count:1000
    ~name:"pretty_correct"
    arbitrary_expr
    (fun expr -> parse (pretty expr) = expr)


(** {1 Entrypoint of test harness} *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [compile_correct; pretty_correct]
  in
  Alcotest.run "compile-arithcond" [
    "properties", suite
  ]
