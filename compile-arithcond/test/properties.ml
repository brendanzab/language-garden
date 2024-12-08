(** Test some properties of the compiler *)

module Tree_lang = Arith_cond.Tree_lang
module Stack_lang = Arith_cond.Stack_lang
module Anf_lang = Arith_cond.Anf_lang


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
    oneofl ['-'; '_'];
    char_range 'a' 'z';
    char_range 'A' 'Z';
    char_range '0' '9';
  ] in

  map2 (Format.sprintf "%c%s") start (small_string ~gen:continue)
    (* not sure if there's a nicer way to do this! *)
    |> map (fun n -> if is_keyword n then n ^ "_" else n)

(** Type generation *)
let gen_ty : Tree_lang.ty QCheck.Gen.t =
  QCheck.Gen.oneofl [
    Tree_lang.Ty_int;
    Tree_lang.Ty_bool;
  ]

(** Random term generator (well-scoped, but probably ill-typed) *)
let gen_expr_untyped : Tree_lang.expr QCheck.Gen.t =
  let open QCheck.Gen in

  let var_freqs size =
    if size <= 0 then [] else
      [1, map Tree_lang.var (int_range 0 (size - 1))]
  in
  let gen_lit = oneof [
    map Tree_lang.int big_nat;
    map Tree_lang.bool bool;
  ] in

  pair (pure 0) nat >>= fix
    (fun self -> function
      | size, 0 ->
          frequency (var_freqs size @ [1, gen_lit])
      | size, n ->
          frequency (var_freqs size @ [
            1, gen_lit;
            2, map Tree_lang.neg (self (size, n/2));
            3, map2 Tree_lang.add (self (size, n/2)) (self (size, n/2));
            3, map2 Tree_lang.sub (self (size, n/2)) (self (size, n/2));
            3, map2 Tree_lang.mul (self (size, n/2)) (self (size, n/2));
            3, map2 Tree_lang.div (self (size, n/2)) (self (size, n/2));
            3, map2 Tree_lang.eq (self (size, n/2)) (self (size, n/2));
            4, map3 Tree_lang.if_then_else (self (size, n/2)) (self (size, n/2)) (self (size, n/2));
            3, map3 Tree_lang.let_ gen_name (self (size, n/2)) (self (size + 1, n/2));
          ])
      )

(** An environment for generating well-typed terms *)
module Env = struct

  module Name_map = Map.Make (String)
  module Name_set = Set.Make (String)
  module Type_map = Map.Make (struct
    type t = Tree_lang.ty
    let compare = compare
  end)

  type t = {
    size : int;                     (** Total number of bindings in scope *)
    name_levels : int Name_map.t;    (** Mappings from names to levels *)
    ty_names : Name_set.t Type_map.t; (** Mappings from types to name sets *)
  }

  (** Empty environment *)
  let empty = {
    size = 0;
    name_levels = Name_map.empty;
    ty_names = Type_map.empty;
  }

  (** Add a new binding to the environment *)
  let add env n t = {
    size = env.size + 1;
    name_levels = Name_map.add n env.size env.name_levels;
    ty_names =
      let ty_names =
        (* Remove shadowed bindings *)
        if Name_map.mem n env.name_levels then
          env.ty_names |> Type_map.filter_map (fun _ names ->
            let names = Name_set.remove n names in
            if Name_set.is_empty names then None else Some names)
        else
          (* Fast path, if the name was not bound *)
          env.ty_names
      in
      ty_names |> Type_map.update t (function
        | Some names -> Some (Name_set.add n names)
        | None -> Some (Name_set.singleton n));
  }

  (** Create a variable at the given scope *)
  let var env level =
    Tree_lang.var (env.size - level - 1)

  (** Constructs a generator for variables of a given type. Returns [None] if no
      variable of that type is bound in the environment. *)
  let gen_var env t =
    let open QCheck.Gen in
    Type_map.find_opt t env.ty_names
      |> Option.map (fun names ->
        let+ n = delay (fun () -> oneofl (Name_set.elements names)) in
        var env (Name_map.find n env.name_levels))

end


(** Random term generator (well-typed) *)
let gen_expr gen_ty : Tree_lang.expr QCheck.Gen.t =
  let open QCheck.Gen in

  let gen_lit = function
    | Tree_lang.Ty_int -> map Tree_lang.int big_nat
    | Tree_lang.Ty_bool -> map Tree_lang.bool bool
  in
  let gen_let self (env, ty, n) =
    let* def_name = gen_name in
    let* def_ty = gen_ty in
    Tree_lang.let_ def_name
      <$> self (env, def_ty, n/2)
      <*> delay (fun () -> self (Env.add env def_name def_ty, ty, n/2))
  in
  let gen_if self (env, ty, n) =
    Tree_lang.if_then_else
      <$> (self (env, Tree_lang.Ty_bool, n/2))
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
      | env, Tree_lang.Ty_int, n ->
          frequency [
            3, gen_let self (env, Tree_lang.Ty_int, n);
            1, gen_lit Tree_lang.Ty_int;
            1, gen_var env Tree_lang.Ty_int;
            2, map Tree_lang.neg (self (env, Tree_lang.Ty_int, n/2));
            3, map2 Tree_lang.add (self (env, Tree_lang.Ty_int, n/2)) (self (env, Tree_lang.Ty_int, n/2));
            3, map2 Tree_lang.sub (self (env, Tree_lang.Ty_int, n/2)) (self (env, Tree_lang.Ty_int, n/2));
            3, map2 Tree_lang.mul (self (env, Tree_lang.Ty_int, n/2)) (self (env, Tree_lang.Ty_int, n/2));
            3, map2 Tree_lang.div (self (env, Tree_lang.Ty_int, n/2)) (self (env, Tree_lang.Ty_int, n/2));
            4, gen_if self (env, Tree_lang.Ty_int, n);
          ]
      | env, Tree_lang.Ty_bool, n ->
          frequency [
            3, gen_let self (env, Tree_lang.Ty_bool, n);
            1, gen_lit Tree_lang.Ty_bool;
            1, gen_var env Tree_lang.Ty_bool;
            3, (let* ty = gen_ty in map2 Tree_lang.eq (self (env, ty, n/2)) (self (env, ty, n/2)));
            4, gen_if self (env, Tree_lang.Ty_bool, n);
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
  (* TODO: bidirectional, type-directed shrinking *)
  QCheck.Iter.(function
  | Tree_lang.Var _ -> QCheck.Iter.empty
  | Tree_lang.Let (n, e1, e2) ->
      (* don't shrink to the body - it depends on the definition *)
      (* FIXME: shrinking can result in ill-typed expressions because the
         definition might not have the same type as the body *)
      (* TODO: shrink to the body if the var is not bound in the term *)
      (* TODO: shrink the body by substituting the definition into the body *)
      QCheck.Iter.of_list [e1]
        <+> (let+ n' = shrink_name n in Tree_lang.let_ n' e1 e2)
        <+> (let+ e1' = shrink_expr e1 in Tree_lang.let_ n e1' e2)
        <+> (let+ e2' = shrink_expr e2 in Tree_lang.let_ n e1 e2')
  | Tree_lang.Int i -> map Tree_lang.int (QCheck.Shrink.int i)
  | Tree_lang.Bool b -> map Tree_lang.bool (shrink_bool b)
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
        <+> (let+ e2' = shrink_expr e2 in Tree_lang.div e1 e2')
  | Tree_lang.Eq (e1, e2) ->
      QCheck.Iter.empty (* subexpressions might not be booleans, so don't shrink to them *)
        <+> (let+ e1' = shrink_expr e1 in Tree_lang.eq e1' e2)
        <+> (let+ e2' = shrink_expr e2 in Tree_lang.eq e1 e2')
  | Tree_lang.If_then_else (e1, e2, e3) ->
      (* shrink to either branch of the conditional, but not the boolean expression *)
      QCheck.Iter.of_list [e2; e3]
        <+> (let+ e1' = shrink_expr e1 in Tree_lang.if_then_else e1' e2 e3)
        <+> (let+ e2' = shrink_expr e2 in Tree_lang.if_then_else e1 e2' e3)
        <+> (let+ e3' = shrink_expr e3 in Tree_lang.if_then_else e1 e2 e3'))


(** {1 Arbitrary generators + shrinkers + printers} *)

let arbitrary_expr_untyped =
  QCheck.make gen_expr_untyped
    ~print:(Format.asprintf "%a" (Tree_lang.pp_expr []))
    ~shrink:shrink_expr

let arbitrary_expr gen_ty =
  let gen = QCheck.Gen.(let* t = gen_ty in pair (gen_expr (pure t)) (pure t)) in
  let print (e, t) = Format.asprintf "@[@[@[%a@]@ :@]@ %a@]" (Tree_lang.pp_expr []) e Tree_lang.pp_ty t in
  let shrink (e, t) = QCheck.Iter.(let+ e' = shrink_expr e in e', t) in
  QCheck.make gen ~print ~shrink


(** {1 Properties} *)

(* TODO: improve handling of divide-by-zero in evaluators? *)
let catch_div_by_zero f =
  try Ok (f ()) with Division_by_zero as e -> Error e

(** The compiler preserves the semantics of the arithmetic expressions *)
let compile_stack_correct =
  (* TODO: improve handling of divide-by-zero *)
  let eval_tree e = catch_div_by_zero Tree_lang.Semantics.(fun () -> eval [] e) in
  let eval_stack e = catch_div_by_zero Stack_lang.Semantics.(fun () -> eval (e, [], [])) in
  let to_stack = Result.map (function
    | Tree_lang.Semantics.Int n -> [Stack_lang.Semantics.Int n]
    | Tree_lang.Semantics.Bool b -> [Stack_lang.Semantics.Bool b])
  in

  QCheck.Test.make ~count:1000
    ~name:"compile_stack_correct"
    (arbitrary_expr gen_ty)
    (fun (e, _) ->
      eval_stack (Arith_cond.Tree_to_stack.translate e) = to_stack (eval_tree e))

(** Compilation preserves the semantics of the arithmetic expressions *)
let compile_anf_correct =
  let eval_tree e = catch_div_by_zero Tree_lang.Semantics.(fun () -> eval [] e) in
  let eval_anf e = catch_div_by_zero Anf_lang.Semantics.(fun () -> eval Env.empty e) in
  let to_anf = Result.map (function
    | Tree_lang.Semantics.Int n -> Anf_lang.Semantics.Int n
    | Tree_lang.Semantics.Bool b -> Anf_lang.Semantics.Bool b)
  in

  QCheck.Test.make ~count:1000
    ~name:"compile_anf_correct"
    (arbitrary_expr gen_ty)
    (fun (e, _) ->
      eval_anf (Arith_cond.Tree_to_anf.translate e) = to_anf (eval_tree e))

(** All well-typed expressions should pass the type checker *)
let check_correct =
  QCheck.Test.make ~count:1000
    ~name:"check_correct"
    (arbitrary_expr gen_ty)
    (fun (e, t) -> Tree_lang.Validation.check [] e t; true)

(* TODO: Test progress and preservation *)
(* TODO: Test errors *)

(** Pretty printed expression can always be parsed back into the same expression. *)
let pretty_correct =
  let pretty e = Format.asprintf "%a" (Tree_lang.pp_expr []) e in
  let parse source =
    Sedlexing.Utf8.from_string source
    |> Sedlexing.with_tokenizer Tree_lang.Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Tree_lang.Parser.main
  in

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
