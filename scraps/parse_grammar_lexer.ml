(** A simple grammar specification language with support for custom lexers.
    It’s capable of parsing and running its own grammar.
*)

(* TODO: Lexer grammar *)
(* TODO: Derivative-based lexers *)
(* TODO: Labelled subrules *)
(* TODO: Typed parse trees *)

module Span = struct

  type t = {
    start : int;
    stop : int;
  }

end

module Token = struct

  type t = {
    name : string;
    span : Span.t;
    contents : string;
  }

end

(** Dynamically typed concrete syntax trees

    Inspired by https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html#Designing-the-Tree
*)
module Parse_tree = struct

  type t = {
    name : string;
    children : child list;
  }

  and child =
    | Token of Token.t
    | Tree of t

end

(** Lexer specification *)
module Lexer : sig

  type t

  (** Regular expressions *)
  type expr

  val fail : expr [@@warning "-unused-value-declaration"]
  val empty : expr [@@warning "-unused-value-declaration"]
  val any : expr
  val char : char -> expr
  val range : char -> char -> expr
  val seq : expr list -> expr
  val alt : expr list -> expr
  val opt : expr -> expr [@@warning "-unused-value-declaration"]
  val rep0 : expr -> expr
  val rep1 : expr -> expr
  val chars : string -> expr [@@warning "-unused-value-declaration"]
  val string : string -> expr
  val not : expr -> expr

  type rule

  val return : expr -> string -> rule
  val skip : expr -> rule

  val create : rule array -> t

  exception Error of { pos : int }

  val tokenise : t -> string -> Token.t Seq.t

end = struct

  type expr =
    | Fail
    | Empty
    | Any
    | Char of char
    | Range of char * char
    | Seq of expr * expr
    | Alt of expr * expr
    | Opt of expr
    | Rep0 of expr
    | Not of expr

  let fail = Fail
  let empty = Empty
  let any = Any
  let char ch = Char ch
  let range ch1 ch2 = Range (ch1, ch2)

  let rec seq = function
    | [] -> empty
    | [e] -> e
    | e :: es -> Seq (e, seq es)

  let rec alt = function
    | [] -> fail
    | [e] -> e
    | e :: es -> Alt (e, alt es)

  let opt e = Opt e
  let rep0 e = Rep0 e
  let rep1 e = Seq (e, Rep0 e)
  let chars s = alt (String.to_seq s |> Seq.map char |> List.of_seq)
  let string s = seq (String.to_seq s |> Seq.map char |> List.of_seq)
  let not e = Not e

  (** Inspired by {{: https://doi.org/10.1145/3591269} flap} *)
  type rule =
    | Return of expr * string
    | Skip of expr

  let return e t = Return (e, t)
  let skip e = Skip e

  type t = {
    rules : rule array;
  }

  let create rules = { rules }

  let ( let* ) = Option.bind
  let ( let+ ) x f = Option.map f x

  let rec consume_expr (e : expr) (pos : int) (input : string) : int option =
    (* TODO: Would be more efficient to use derivatives or compile this to a DFA.

       - Regular-expression derivatives re-examined https://doi.org/10.1017/S0956796808007090
       - Fix-ing regular expressions https://well-typed.com/blog/2020/06/fix-ing-regular-expressions/
       - https://github.com/ulysses4ever/rere/blob/master/rere.m
    *)
    match e with
    | Fail -> None
    | Empty -> Some pos
    | Any ->
        begin match String.get input pos with
        | _ -> Some (pos + 1)
        | exception Invalid_argument _ -> None
        end
    | Char expected ->
        begin match String.get input pos with
        | ch when ch = expected -> Some (pos + 1)
        | _ | exception Invalid_argument _ -> None
        end
    | Range (min, max) ->
        begin match String.get input pos with
        | ch when min <= ch && ch <= max -> Some (pos + 1)
        | _ | exception Invalid_argument _ -> None
        end
    | Seq (e1, e2) ->
        let* pos = consume_expr e1 pos input in
        consume_expr e2 pos input
    | Alt (e1, e2) ->
        begin match consume_expr e1 pos input with
        | Some pos -> Some pos
        | None -> consume_expr e2 pos input
        end
    | Opt e ->
        begin match consume_expr e pos input with
        | Some pos -> Some pos
        | None -> Some pos
        end
    | Rep0 e ->
        begin match consume_expr e pos input with
        | Some pos -> consume_expr (Rep0 e) pos input
        | None -> Some pos
        end
    | Not e ->
        begin match consume_expr e pos input with
        | Some _ -> None
        | None -> Some pos
        end

  exception Error of { pos : int }

  let tokenise (lexer : t) (input : string) =
    let[@tail_mod_cons] rec go (lexer : t) (start : int) (input : string) () =
      match
        lexer.rules |> Array.find_map @@ function
          | Return (e, name) ->
              let+ stop = consume_expr e start input in
              let contents = String.sub input start (stop - start) in
              Some Token.{ name; span = { start; stop }; contents }, stop
          | Skip e ->
              let+ stop = consume_expr e start input in
              None, stop
      with
      | Some (Some t, stop) -> Seq.Cons (t, go lexer stop input)
      | Some (None, stop) -> go lexer stop input ()
      | None when start = String.length input -> Seq.Nil
      | None -> raise (Error { pos = start })
    in
    go lexer 0 input

end

module Rule = struct

  type t =
    | Item of string
    | Token of string
    | Seq of t list
    | Alt of t list
    | Opt of t
    | Rep0 of t
    | Rep1 of t

end

module Grammar = struct

  type t = {
    items : (string * Rule.t) list;
  }

end

module Parser : sig

  type error_data =
    | Unexpected_eof
    | Unexpected_token of { found : Token.t }

  exception Error of error_data

  val parse_grammar : Token.t Seq.t -> Grammar.t

end = struct

  type error_data =
    | Unexpected_eof
    | Unexpected_token of { found : Token.t }

  exception Error of error_data

  let expect (token_name : string) (tokens : Token.t Seq.t) : Token.t Seq.t =
    match Seq.uncons tokens with
    | Some (t, tokens) when t.name = token_name -> tokens
    | Some (token, _) -> raise (Error (Unexpected_token { found = token }))
    | None -> raise (Error Unexpected_eof)

  let expect_ident (tokens : Token.t Seq.t) : string * Token.t Seq.t =
    match Seq.uncons tokens with
    | Some ({ name = "IDENT"; contents; _ }, tokens) -> contents, tokens
    | Some (token, _) -> raise (Error (Unexpected_token { found = token }))
    | None -> raise (Error Unexpected_eof)

  let rec parse_rule (tokens : Token.t Seq.t) : Rule.t * Token.t Seq.t =
    let tokens =
      match Seq.uncons tokens with
      | Some ({ name = "|"; _ }, tokens) -> tokens
      | _ -> tokens
    in
    let rule, tokens = parse_seq_rule tokens in
    let rules, tokens =
      let rec go (acc : Rule.t list) (tokens : Token.t Seq.t) =
        match Seq.uncons tokens with
        | Some ({ name = "|"; _ }, tokens) ->
            let rule, tokens = parse_seq_rule tokens in
            (go [@tailcall]) (rule :: acc) tokens
        | _ -> List.rev acc, tokens
      in
      go [] tokens
    in
    match (rule :: rules) with
    | [rule] -> rule, tokens
    | rules -> Rule.Alt rules, tokens

  and parse_seq_rule (tokens : Token.t Seq.t) : Rule.t * Token.t Seq.t =
    let rule, tokens = parse_rep_rule tokens in
    let rules, tokens =
      let rec go (acc : Rule.t list) (tokens : Token.t Seq.t) =
        match Seq.uncons tokens with
        | Some ({ name = "IDENT" | "STRING" | "("; _ }, _) ->
            let rule, tokens = parse_rep_rule tokens in
            (go [@tailcall]) (rule :: acc) tokens
        | _ -> List.rev acc, tokens
      in
      go [] tokens
    in
    match (rule :: rules) with
    | [rule] -> rule, tokens
    | rules -> Rule.Seq rules, tokens

  and parse_rep_rule (tokens : Token.t Seq.t) : Rule.t * Token.t Seq.t =
    let rule, tokens = parse_atom_rule tokens in
    match Seq.uncons tokens with
    | Some ({ name = "*"; _ }, tokens) -> Rule.Rep0 rule, tokens
    | Some ({ name = "+"; _ }, tokens) -> Rule.Rep1 rule, tokens
    | Some ({ name = "?"; _ }, tokens) -> Rule.Opt rule, tokens
    | _ -> rule, tokens

  and parse_atom_rule (tokens : Token.t Seq.t) : Rule.t * Token.t Seq.t =
    match Seq.uncons tokens with
    | Some ({ name = "IDENT"; contents; _ }, tokens) -> Rule.Item contents, tokens
    | Some ({ name = "STRING"; contents; _ }, tokens) ->
        let s = String.(sub contents 1 (length contents - 2)) in
        Rule.Token (Scanf.unescaped s), tokens
    | Some ({ name = "("; _ }, tokens) ->
        let rule, tokens = parse_rule tokens in
        let tokens = expect ")" tokens in
        rule, tokens
    | Some (token, _) -> raise (Error (Unexpected_token { found = token }))
    | None -> raise (Error Unexpected_eof)

  let parse_item (tokens : Token.t Seq.t) : (string * Rule.t) * Token.t Seq.t =
    let tokens = expect "rule" tokens in
    let ident, tokens = expect_ident tokens in
    let tokens = expect ":=" tokens in
    let rule, tokens = parse_rule tokens in
    (ident, rule), tokens

  let parse_grammar (tokens : Token.t Seq.t) : Grammar.t =
    let[@tail_mod_cons] rec parse_items tokens =
      match Seq.uncons tokens with
      | Some (_, _) ->
          let item, tokens = parse_item tokens in
          item :: parse_items tokens
      | None -> []
    in
    Grammar.{ items = parse_items tokens }

end

(** Top-down, backtracking interpreter for grammars *)
module Semantics = struct

  type error_data =
    | Unmatched_alt
    | Unexpected_token of { found : Token.t }
    | Unexpected_eof
    | Expected_eof

  exception Error of error_data

  let rec parse_rule (grammar : Grammar.t) (rule : Rule.t) (tokens : Token.t Seq.t) : Parse_tree.child list * Token.t Seq.t =
    let rec parse_rule rule acc tokens =
      match rule with
      | Rule.Item name ->
          let tree, tokens = parse_item grammar name tokens in
          Parse_tree.Tree tree :: acc, tokens
      | Rule.Token name ->
          begin match Seq.uncons tokens with
          | Some (found, tokens) when found.name = name -> Parse_tree.Token found :: acc, tokens
          | Some (found, _) -> raise (Error (Unexpected_token { found }))
          | None -> raise (Error Unexpected_eof)
          end
      | Rule.Seq rules ->
          let rec go rules acc tokens =
            match rules with
            | [] -> acc, tokens
            | rule :: rules ->
                let acc, tokens = parse_rule rule acc tokens in
                go rules acc tokens
          in
          go rules acc tokens
      | Rule.Alt rules ->
          let rec go rules =
            match rules with
            | [] -> raise (Error Unmatched_alt)
            | rule :: rules ->
                begin try parse_rule rule acc tokens with
                | Error _ -> go rules
                end
          in
          go rules
      | Rule.Opt rule ->
          begin try parse_rule rule acc tokens with
          | Error _ -> acc, tokens
          end
      | Rule.Rep0 rule ->
          let rec go acc tokens =
            match parse_rule rule acc tokens with
            | acc, tokens -> go acc tokens
            | exception Error _ -> acc, tokens
          in
          go acc tokens
      | Rule.Rep1 rule ->
          let acc, tokens = parse_rule rule acc tokens in
          parse_rule (Rep0 rule) acc tokens
    in
    let rev_children, tokens = parse_rule rule [] tokens in
    List.rev rev_children, tokens

  and parse_item (grammar : Grammar.t) (name : string) (tokens : Token.t Seq.t) : Parse_tree.t * Token.t Seq.t =
    let children, tokens = parse_rule grammar (List.assoc name grammar.items) tokens in
    Parse_tree.{ name; children }, tokens

  let parse_grammar (grammar : Grammar.t) (entrypoint : string) (tokens : Token.t Seq.t) : Parse_tree.t =
    match parse_item grammar entrypoint tokens with
    | tree, tokens when Seq.is_empty tokens -> tree
    | _ -> raise (Error Expected_eof)

end

module Examples = struct

  module Grammar = struct

    let lexer =
      let whitespace = Lexer.(rep1 (alt [char ' '; char '\n'; char '\r'; char '\t'])) in
      let line_comment = Lexer.(seq [string "--"; rep0 (seq [not (alt [char '\r'; char '\n']); any])]) in

      let ident_start = Lexer.(alt [range 'a' 'z'; range 'A' 'Z']) in
      let ident_continue = Lexer.(alt [ident_start; range '0' '9'; char '-'; char '_']) in
      let ident = Lexer.(seq [ident_start; rep0 ident_continue]) in

      let escape_code = Lexer.(seq [char '\\'; alt [char '\\'; char '"']]) in
      let string_lit = Lexer.(seq [char '"'; rep0 (alt [escape_code; seq [not (char '"'); any]]); char '"']) in

      Lexer.(create [|
        skip whitespace;
        skip line_comment;
        return (string "rule") "rule";
        return ident "IDENT";
        return string_lit "STRING";
        return (string "*") "*";
        return (string ":=") ":=";
        return (string "|") "|";
        return (string "+") "+";
        return (string "?") "?";
        return (string "(") "(";
        return (string ")") ")";
      |])

    let grammar = {|

      -- Grammar of grammar specifications

      rule grammar    := item*
      rule item       := "rule" "IDENT" ":=" alt_rule

      rule alt_rule   := "|"? seq_rule ("|" seq_rule)*
      rule seq_rule   := rep_rule+
      rule rep_rule   := atom_rule ("*" | "+" | "?")?
      rule atom_rule  := "IDENT" | "STRING" | "(" alt_rule ")"

    |}

    let rec decode_rule ({ name; children } : Parse_tree.t) : Rule.t =
      let rec decode_alt_rules rules =
        match rules with
        | Parse_tree.Token { name = "|"; _ } :: rules -> decode_alt_rules rules
        | Parse_tree.Token _ :: _ -> invalid_arg "decode_rule"
        | Parse_tree.Tree tree :: rules -> decode_rule tree :: decode_alt_rules rules
        | [] -> []

      and decode_seq_rules rules =
        rules |> List.map @@ function
          | Parse_tree.Tree tree -> decode_rule tree
          | _ -> invalid_arg "decode_rule"
      in
      match name, children with
      | "alt_rule", [Tree rule] -> decode_rule rule
      | "seq_rule", [Tree rule] -> decode_rule rule
      | "alt_rule", rules -> Alt (decode_alt_rules rules)
      | "seq_rule", rules -> Seq (decode_seq_rules rules)
      | "rep_rule", [Tree rule; Token { name = "*"; _ }] -> Rep0 (decode_rule rule)
      | "rep_rule", [Tree rule; Token { name = "+"; _ }] -> Rep1 (decode_rule rule)
      | "rep_rule", [Tree rule; Token { name = "?"; _ }] -> Opt (decode_rule rule)
      | "rep_rule", [Tree rule] -> decode_rule rule
      | "atom_rule", [Token { name = "IDENT"; contents; _ }] -> Item contents
      | "atom_rule", [Token { name = "STRING"; contents = s; _ }] -> Token (String.(sub s 1 (length s - 2)))
      | "atom_rule", [Token { name = "("; _ }; Tree rule; Token { name = ")"; _ }] -> decode_rule rule
      | _ -> invalid_arg "decode_rule"

    let decode_grammar ({ name; children } : Parse_tree.t) : Grammar.t =
      let decode_item ({ name; children } : Parse_tree.t) : string * Rule.t =
        match name, children with
        | "item", [Token { name = "rule"; _ }; Token ident; Token { name = ":="; _ }; Tree alt_rule] ->
            ident.contents, decode_rule alt_rule
        | _ -> invalid_arg "decode_grammar"
      in
      match name, children with
      | "grammar", items ->
          let items =
            items |> List.map @@ function
              | Parse_tree.Tree tree -> decode_item tree
              | _ -> invalid_arg "decode_grammar"
          in
          Grammar.{ items }
      | _ -> invalid_arg "decode_grammar"

  end

  module Sexpr = struct

    let lexer =
      let whitespace = Lexer.(rep1 (alt [char ' '; char '\n'; char '\r'; char '\t'])) in
      let atom = Lexer.(rep1 (range 'a' 'z')) in

      Lexer.(create [|
        skip whitespace;
        return atom "ATOM";
        return (string "(") "LEFT_PAREN";
        return (string ")") "RIGHT_PAREN";
      |])

    let grammar = {|

      rule sexpr :=
        | "LEFT_PAREN" sexpr* "RIGHT_PAREN"
        | "ATOM"

    |}

    type sexpr =
      | Atom of string
      | List of sexpr list

    let rec decode_sexpr ({ name; children } : Parse_tree.t) : sexpr =
      match name, children with
      | "sexpr", [Token { name = "ATOM"; contents; _ }]  -> Atom contents
      | "sexpr", Token { name = "LEFT_PAREN"; _ } :: children ->
          let[@tail_mod_cons] rec go : Parse_tree.child list -> sexpr list = function
            | [Token { name = "RIGHT_PAREN"; _ }] -> []
            | Tree e :: children -> decode_sexpr e :: go children
            | _ -> invalid_arg "decode_sexpr"
          in
          List (go children)
      | _ -> invalid_arg "decode_sexpr"

  end

  module Arith = struct

    let lexer =
      let whitespace = Lexer.(rep1 (alt [char ' '; char '\n'; char '\r'; char '\t'])) in
      let line_comment = Lexer.(seq [string "--"; rep0 (seq [not (alt [char '\r'; char '\n']); any])]) in

      let ident_start = Lexer.(alt [range 'a' 'z'; range 'A' 'Z']) in
      let ident_continue = Lexer.(alt [ident_start; range '0' '9'; char '-'; char '_']) in
      let ident = Lexer.(seq [ident_start; rep0 ident_continue]) in

      let number = Lexer.(rep1 (range '0' '9')) in

      Lexer.(create [|
        skip whitespace;
        skip line_comment;
        return ident "IDENT";
        return number "NUMBER";
        return (string "*") "*";
        return (string "/") "/";
        return (string "-") "-";
        return (string "+") "+";
        return (string "(") "(";
        return (string ")") ")";
      |])

    let grammar = {|

      -- Arithmetic expressions

      rule expr :=
        | mul_expr (("+" | "-") mul_expr)*

      rule mul_expr :=
        | prefix_expr (("*" | "/") prefix_expr)*

      rule prefix_expr :=
        | ("+" | "-")? atom_expr

      rule atom_expr :=
        | "IDENT"
        | "NUMBER"
        | "(" expr ")"

    |}

    type expr =
      | Var of string
      | Num of int
      | Add of expr * expr
      | Sub of expr * expr
      | Mul of expr * expr
      | Div of expr * expr
      | Pos of expr
      | Neg of expr

    let rec decode_expr ({ name; children } : Parse_tree.t) : expr =
      match name, children with
      | "expr", children ->
          let rec go (children : Parse_tree.child list) =
            match children with
            | Tree expr :: Token { name = "+"; _ } :: children -> Add (decode_expr expr, go children)
            | Tree expr :: Token { name = "-"; _ } :: children -> Sub (decode_expr expr, go children)
            | [Tree expr] -> decode_expr expr
            |  _ -> invalid_arg "decode_expr"
          in
          go children
      | "mul_expr", children ->
          let rec go (children : Parse_tree.child list) =
            match children with
            | Tree expr :: Token { name = "*"; _ } :: children -> Mul (decode_expr expr, go children)
            | Tree expr :: Token { name = "/"; _ } :: children -> Div (decode_expr expr, go children)
            | [Tree expr] -> decode_expr expr
            |  _ -> invalid_arg "decode_expr"
          in
          go children
      | "prefix_expr", [Token { name = "+"; _ }; Tree expr] -> Pos (decode_expr expr)
      | "prefix_expr", [Token { name = "-"; _ }; Tree expr] -> Neg (decode_expr expr)
      | "prefix_expr", [Tree expr] -> decode_expr expr
      | "atom_expr", [Token { name = "IDENT"; contents; _ }] -> Var contents
      | "atom_expr", [Token { name = "NUMBER"; contents; _ }] -> Num (int_of_string contents)
      | "atom_expr", [Token { name = "("; _ }; Tree expr; Token { name = ")"; _ }] -> decode_expr expr
      | _ -> invalid_arg "decode_expr"

  end


  module Pl0 = struct

    let grammar = {|

      -- Wirth’s PL/0 language from “Algorithms + Data Structures = Programs”.
      --
      -- * https://en.wikipedia.org/wiki/PL/0#Grammar
      -- * https://en.wikipedia.org/wiki/Recursive_descent_parser#Example_parser

      rule program :=
        | block "."

      rule block :=
        ("const" "IDENT" "=" "NUMBER" ("," "IDENT" "=" "NUMBER")* ";")?
        ("var" "IDENT" ("," "IDENT")* ";")?
        ("procedure" "IDENT" ";" block ";")* statement

      rule statement :=
        | "IDENT" ":=" expression
        | "call" "IDENT"
        | "begin" statement (";" statement)* "end"
        | "if" condition "then" statement
        | "while" condition "do" statement

      rule condition :=
        | "odd" expression
        | expression ("=" | "#" | "<" | "<=" | ">" | ">=") expression

      rule expression :=
        | ("+" | "-")? term (("+" | "-") term)?

      rule term :=
        | factor (("*" | "/") factor)*

      rule factor :=
        | "IDENT"
        | "NUMBER"
        | "(" expression ")"

    |}

  end

end

let sexpr_tests (parse_grammar : string -> Grammar.t) = begin
  let sexpr_grammar = parse_grammar Examples.Sexpr.grammar in
  let parse_sexpr (input : string) : Examples.Sexpr.sexpr =
    Lexer.tokenise Examples.Sexpr.lexer input
    |> Semantics.parse_grammar sexpr_grammar "sexpr"
    |> Examples.Sexpr.decode_sexpr
  in

  assert (parse_sexpr "hello" = Atom "hello");
  assert (parse_sexpr "()" = List []);
  assert (parse_sexpr "(hello world)" = List [Atom "hello"; Atom "world"]);
  assert (parse_sexpr "(defn const (x y) x)" = List [Atom "defn"; Atom "const"; List [Atom "x"; Atom "y"]; Atom "x"]);

end

let arith_tests (parse_grammar : string -> Grammar.t) = begin
  let arith_grammar = parse_grammar Examples.Arith.grammar in
  let parse_arith (input : string) : Examples.Arith.expr =
    Lexer.tokenise Examples.Arith.lexer input
    |> Semantics.parse_grammar arith_grammar "expr"
    |> Examples.Arith.decode_expr
  in

  assert (parse_arith "42" = Num 42);
  assert (parse_arith "pi" = Var "pi");
  assert (parse_arith "3 * x + y / 25" = Add (Mul (Num 3, Var "x"), Div (Var "y", Num 25)));
  assert (parse_arith "(x + y) * +25" = Mul (Add (Var "x", Var "y"), Pos (Num 25)));

end

let pl0_tests (parse_grammar : string -> Grammar.t) = begin

  let pl0_grammar = parse_grammar Examples.Pl0.grammar in

  (* TODO: Improve tests *)
  ignore pl0_grammar;

end

let () = begin

  Printexc.record_backtrace true;

  Printexc.register_printer begin function
    | Parser.Error Unexpected_eof -> Some "Parser.Unexpected_eof"
    | Parser.Error (Unexpected_token { found = { name; span; contents} }) ->
        Some (Printf.sprintf "Parser.Unexpected_token: %s @ %i ..%i := \"%s\""
          name span.start span.stop (String.escaped contents))

    | Semantics.Error Unmatched_alt -> Some "Semantics.Unmatched_alt"
    | Semantics.Error (Unexpected_token { found = { name; span; contents} }) ->
        Some (Printf.sprintf "Semantics.Unexpected_token: %s @ %i ..%i := \"%s\""
          name span.start span.stop (String.escaped contents))
    | Semantics.Error Unexpected_eof -> Some "Semantics.Unexpected_eof"
    | Semantics.Error Expected_eof -> Some "Semantics.Expected_eof"

    | _ -> None
  end;

  print_string "Running tests ...";

  (* Parse a grammar with the bootstrap grammar *)
  let parse_grammar (input : string) : Grammar.t =
    Lexer.tokenise Examples.Grammar.lexer input
    |> Parser.parse_grammar
  in

  sexpr_tests parse_grammar;
  arith_tests parse_grammar;
  pl0_tests parse_grammar;

  (* Parse the grammar of grammars with the bootstrap parser *)
  let grammar_grammar = parse_grammar Examples.Grammar.grammar in

  (* Parse a grammar with the parsed grammar AST *)
  let parse_grammar' (input : string) : Grammar.t =
    Lexer.tokenise Examples.Grammar.lexer input
    |> Semantics.parse_grammar grammar_grammar "grammar"
    |> Examples.Grammar.decode_grammar
  in

  sexpr_tests parse_grammar';
  arith_tests parse_grammar';
  pl0_tests parse_grammar';

  assert (grammar_grammar = parse_grammar' Examples.Grammar.grammar);

  print_string " ok!\n";

end
