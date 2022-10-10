(** Test some properties of the compiler *)

module TreeLang = ArithCond.TreeLang
module StackLang = ArithCond.StackLang
module AnfLang = ArithCond.AnfLang


let is_keyword = function
  | "else"
  | "false"
  | "if"
  | "let"
  | "then"
  | "true" -> true
  | _ -> false


(** {1 Generators} *)

(** Random name generator, avoiding keywords *)
let gen_name : string QCheck.Gen.t =
  let open QCheck.Gen in

  let start = oneof [
    char_range 'a' 'z';
    char_range 'A' 'Z';
  ] in
  let continue = oneof [
    pure '-';
    pure '_';
    char_range 'a' 'z';
    char_range 'A' 'Z';
    char_range '0' '9';
  ] in

  map2 (Format.sprintf "%c%s") start (small_string ~gen:continue)
    (* not sure if there's a nicer way to do this! *)
    |> map (fun n -> if is_keyword n then n ^ "_" else n)

(** Type generation *)
let gen_ty : TreeLang.ty QCheck.Gen.t =
  QCheck.Gen.oneofl [
    TreeLang.TyInt;
    TreeLang.TyBool;
  ]

(** Random term generator (well-scoped, but probably ill-typed) *)
let gen_expr_untyped : TreeLang.expr QCheck.Gen.t =
  let open QCheck.Gen in

  let var_freqs = function
    | size when size <= 0 -> []
    | size -> [1, map TreeLang.var (int_range 0 (size - 1))]
  in
  let gen_lit = oneof [
    map TreeLang.int big_nat;
    map TreeLang.bool bool;
  ] in

  pair (pure 0) nat >>= fix
    (fun self -> function
      | size, 0 ->
          frequency (var_freqs size @ [1, gen_lit])
      | size, n ->
          frequency (var_freqs size @ [
            1, gen_lit;
            2, map TreeLang.neg (self (size, n/2));
            3, map2 TreeLang.add (self (size, n/2)) (self (size, n/2));
            3, map2 TreeLang.sub (self (size, n/2)) (self (size, n/2));
            3, map2 TreeLang.mul (self (size, n/2)) (self (size, n/2));
            3, map2 TreeLang.div (self (size, n/2)) (self (size, n/2));
            3, map2 TreeLang.eq (self (size, n/2)) (self (size, n/2));
            4, map3 TreeLang.if_then_else (self (size, n/2)) (self (size, n/2)) (self (size, n/2));
            3, map3 TreeLang.let_ gen_name (self (size, n/2)) (self (size + 1, n/2));
          ])
      )

(** An environment for generating well-typed terms *)
module Env = struct

  module StringMap = Map.Make (String)
  module StringSet = Set.Make (String)

  type t = {
    size : int;                     (** Total number of bindings in scope *)

    (* NOTE: These will need to be changed if the types become more complicated *)
    int_names : StringSet.t;        (** Integers currently in scope *)
    int_levels : int StringMap.t;   (** The levels of integers in scope *)
    bool_names : StringSet.t;       (** Booleans currently in scope *)
    bool_levels : int StringMap.t;  (** The levels of booleans in scope *)
  }

  let empty = {
    size = 0;
    int_names = StringSet.empty;
    int_levels = StringMap.empty;
    bool_names = StringSet.empty;
    bool_levels = StringMap.empty;
  }

  (** Add a new binding to the environment *)
  let add env name = function
    | TreeLang.TyInt ->
        {
          size = env.size + 1;
          int_names = StringSet.add name env.int_names;
          int_levels = StringMap.add name env.size env.int_levels;
          bool_names = StringSet.remove name env.bool_names;
          bool_levels = StringMap.remove name env.bool_levels;
        }
    | TreeLang.TyBool ->
        {
          size = env.size + 1;
          int_names = StringSet.remove name env.int_names;
          int_levels = StringMap.remove name env.int_levels;
          bool_names = StringSet.add name env.bool_names;
          bool_levels = StringMap.add name env.size env.bool_levels;
        }

  (** Create a variable at the given scope *)
  let var env level =
    TreeLang.var (env.size - level - 1)

  (** Constructs a generator for variables of a given type. Returns [None] if no
      variable of that type is bound in the environment. *)
  let gen_var env t =
    let open QCheck.Gen in
    match t with
    | TreeLang.TyInt when StringSet.cardinal env.int_names > 0 ->
        Some (let+ n = oneofl (StringSet.elements env.int_names) in
          var env (StringMap.find n env.int_levels))
    | TreeLang.TyBool when StringSet.cardinal env.bool_names > 0 ->
        Some (let+ n = oneofl (StringSet.elements env.bool_names) in
          var env (StringMap.find n env.bool_levels))
    | _ -> None

end


(** Random term generator (well-typed) *)
let gen_expr gen_ty : TreeLang.expr QCheck.Gen.t =
  let open QCheck.Gen in

  let gen_lit = function
    | TreeLang.TyInt -> map TreeLang.int big_nat
    | TreeLang.TyBool -> map TreeLang.bool bool
  in
  let gen_let self (env, ty, n) =
    let* def_name = gen_name in
    let* def_ty = gen_ty in
    TreeLang.let_ def_name
      <$> self (env, def_ty, n/2)
      <*> self (Env.add env def_name def_ty, ty, n/2)
  in
  let gen_if self (env, ty, n) =
    TreeLang.if_then_else
      <$> (self (env, TreeLang.TyBool, n/2))
      <*> (self (env, ty, n/2))
      <*> (self (env, ty, n/2))
  in
  let gen_var env ty =
    match Env.gen_var env ty with
    | Some gen -> gen
    | None -> gen_lit ty (* Fallback to literals *)
  in

  (* TODO: bidirectional term generation *)
  triple (pure Env.empty) gen_ty nat >>= fix
    (fun self -> function
      | env, ty, 0 -> oneof [ gen_lit ty; gen_var env ty ]
      | env, TreeLang.TyInt, n ->
          frequency [
            3, gen_let self (env, TreeLang.TyInt, n);
            1, gen_lit TreeLang.TyInt;
            1, gen_var env TreeLang.TyInt;
            2, map TreeLang.neg (self (env, TreeLang.TyInt, n/2));
            3, map2 TreeLang.add (self (env, TreeLang.TyInt, n/2)) (self (env, TreeLang.TyInt, n/2));
            3, map2 TreeLang.sub (self (env, TreeLang.TyInt, n/2)) (self (env, TreeLang.TyInt, n/2));
            3, map2 TreeLang.mul (self (env, TreeLang.TyInt, n/2)) (self (env, TreeLang.TyInt, n/2));
            3, map2 TreeLang.div (self (env, TreeLang.TyInt, n/2)) (self (env, TreeLang.TyInt, n/2));
            4, gen_if self (env, TreeLang.TyInt, n);
          ]
      | env, TreeLang.TyBool, n ->
          frequency [
            3, gen_let self (env, TreeLang.TyBool, n);
            1, gen_lit TreeLang.TyBool;
            1, gen_var env TreeLang.TyBool;
            3, (let* ty = gen_ty in map2 TreeLang.eq (self (env, ty, n/2)) (self (env, ty, n/2)));
            4, gen_if self (env, TreeLang.TyBool, n);
          ]
      )


(** {1 Shrinkers} *)

let shrink_bool b yield =
  if b then yield false else ()

let shrink_name n =
  QCheck.Shrink.string n
    |> QCheck.Iter.filter (fun n ->
      String.length n > 0 && not (is_keyword n))

let rec shrink_expr =
  (* TODO: type-directed shrinking *)
  QCheck.Iter.(function
  | TreeLang.Var _ -> QCheck.Iter.empty
  | TreeLang.Let (n, e1, e2) ->
      (* don't shrink to the body - it depends on the definition *)
      (* FIXME: shrinking can result in ill-typed expressions because the
         definition might not have the same type as the body *)
      (* TODO: shrink the body by substituting the definition into the body *)
      QCheck.Iter.of_list [e1]
        <+> (let+ n' = shrink_name n in TreeLang.let_ n' e1 e2)
        <+> (let+ e1' = shrink_expr e1 in TreeLang.let_ n e1' e2)
        <+> (let+ e2' = shrink_expr e2 in TreeLang.let_ n e1 e2')
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
      QCheck.Iter.empty (* subexpressions might not be booleans, so don't shrink to them *)
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
    ~print:(Format.asprintf "%a" (TreeLang.pp_expr []))
    ~shrink:shrink_expr

let arbitrary_expr gen_ty =
  let gen = QCheck.Gen.(let* t = gen_ty in pair (gen_expr (pure t)) (pure t)) in
  let print (e, t) = Format.asprintf "@[@[@[%a@]@ :@]@ %a@]" (TreeLang.pp_expr []) e TreeLang.pp_ty t in
  let shrink (e, t) = QCheck.Iter.(let+ e' = shrink_expr e in e', t) in
  QCheck.make gen ~print ~shrink


(** {1 Properties} *)

(* TODO: improve handling of divide-by-zero in evaluators? *)
let catch_div_by_zero f =
  try Ok (f ()) with Division_by_zero as e -> Error e

(** The compiler preserves the semantics of the arithmetic expressions *)
let compile_stack_correct =
  (* TODO: improve handling of divide-by-zero *)
  let eval_tree e = catch_div_by_zero (fun () -> TreeLang.Semantics.eval [] e) in
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
  let eval_tree e = catch_div_by_zero (fun () -> TreeLang.Semantics.eval [] e) in
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
    (fun (e, t) -> TreeLang.Validation.check [] e t; true)

(* TODO: Test progress and preservation *)
(* TODO: Test errors *)

(** Pretty printed expression can always be parsed back into the same expression. *)
let pretty_correct =
  let pretty e = Format.asprintf "%a" (TreeLang.pp_expr []) e in
  let parse source = TreeLang.(Parser.main Lexer.token (Lexing.from_string source)) in

  QCheck.Test.make ~count:1000
    ~name:"pretty_correct"
    arbitrary_expr_untyped
    (fun e ->
      let s = pretty e in
      pretty (parse s) = s)


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
