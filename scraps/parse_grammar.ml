(** Recursive descent parser for a simple grammar specification language. *)

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

module Token = struct

  type t =
    | IDENT of string
    | STRING of string
    | KW_RULE
    | ASTERISK
    | COLON_EQUALS
    | PIPE
    | PLUS
    | QUESTION
    | LEFT_PAREN
    | RIGHT_PAREN

  let ident (token : t) : string =
    match token with
    | IDENT _ -> "IDENT"
    | STRING _ -> "STRING"
    | KW_RULE -> "rule"
    | ASTERISK -> "*"
    | COLON_EQUALS -> ":="
    | PIPE -> "|"
    | PLUS -> "+"
    | QUESTION -> "?"
    | LEFT_PAREN -> "("
    | RIGHT_PAREN -> ")"

end

module Lexer : sig

  type t = Token.t Seq.t

  type error_data =
    | Unexpected_char of { found : char }
    | Unexpected_escape_code of { found : char }
    | Unexpected_eof

  exception Error of error_data

  val tokenise : string -> t

end  = struct

  type t = Token.t Seq.t

  let is_digit (ch : char) : bool =
    match ch with
    | '0' .. '9' -> true
    | _ -> false

  let is_ident_start (ch : char) : bool =
    match ch with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false

  let is_ident_continue (ch : char) : bool =
    is_ident_start ch || is_digit ch

  let is_ascii_whitespace (ch : char) : bool =
    match ch with
    | ' ' | '\n' | '\r' | '\t' -> true
    | _ -> false

  type error_data =
    | Unexpected_char of { found : char }
    | Unexpected_escape_code of { found : char }
    | Unexpected_eof

  exception Error of error_data

  let take_while (f : char -> bool) (input : char Seq.t) : string * char Seq.t =
    let buf = Buffer.create 16 in
    let rec go input =
      match Seq.uncons input with
      | Some (ch, input) when f ch ->
          Buffer.add_char buf ch;
          (go [@tailcall]) input
      | Some _ | None ->
          (String.of_bytes (Buffer.to_bytes buf), input)
    in
    go input

  let quoted (input : char Seq.t) : string * char Seq.t =
    let buf = Buffer.create 16 in
    let rec go input =
      match Seq.uncons input with
      | Some ('"', input) ->
          String.of_bytes (Buffer.to_bytes buf), input
      | Some ('\\', input) ->
          begin match Seq.uncons input with
          | Some ('\\'| '"' as ch, input) ->
              Buffer.add_char buf ch;
              (go [@tailcall]) input
          | Some (ch, _) -> raise (Error (Unexpected_escape_code { found = ch }))
          | None -> raise (Error Unexpected_eof)
          end
      | Some (ch, input) ->
          Buffer.add_char buf ch;
          (go [@tailcall]) input
      | None ->
          raise (Error Unexpected_eof)
    in
    go input

  let[@tail_mod_cons] rec tokens (input : char Seq.t) () =
    match Seq.uncons input with
    | Some ('*', input) -> Seq.Cons (Token.ASTERISK, tokens input)
    | Some (':', input) ->
        begin match Seq.uncons input with
        | Some ('=', input) -> Seq.Cons (Token.COLON_EQUALS, tokens input)
        | Some (ch, _) -> raise (Error (Unexpected_char { found = ch }))
        | None -> raise (Error Unexpected_eof)
        end
    | Some ('|', input) -> Seq.Cons (Token.PIPE, tokens input)
    | Some ('+', input) -> Seq.Cons (Token.PLUS, tokens input)
    | Some ('?', input) -> Seq.Cons (Token.QUESTION, tokens input)
    | Some ('(', input) -> Seq.Cons (Token.LEFT_PAREN, tokens input)
    | Some (')', input) -> Seq.Cons (Token.RIGHT_PAREN, tokens input)
    | Some ('"', input) ->
        let ident, input = quoted input in
        Seq.Cons (Token.STRING ident, tokens input)
    | Some (ch, _) when is_ident_start ch ->
        begin match take_while is_ident_continue input with
        | "rule", input -> Seq.Cons (Token.KW_RULE, tokens input)
        | ident, input -> Seq.Cons (Token.IDENT ident, tokens input)
        end
    | Some (ch, input) when is_ascii_whitespace ch -> tokens input ()
    | Some (ch, _) -> raise (Error (Unexpected_char { found = ch }))
    | None -> Seq.Nil

  let tokenise (input : string) : t =
    tokens (String.to_seq input)

end

module Parser : sig

  type error_data =
    | Unexpected_eof
    | Unexpected_token of { found : Token.t }

  exception Error of error_data

  val parse_grammar : Lexer.t -> Grammar.t

end = struct

  type error_data =
    | Unexpected_eof
    | Unexpected_token of { found : Token.t }

  exception Error of error_data

  let expect (token : Token.t) (tokens : Lexer.t) : Lexer.t =
    match Seq.uncons tokens with
    | Some (t, tokens) when t = token -> tokens
    | Some (token, _) -> raise (Error (Unexpected_token { found = token }))
    | None -> raise (Error Unexpected_eof)

  let expect_ident (tokens : Lexer.t) : string * Lexer.t =
    match Seq.uncons tokens with
    | Some (IDENT ident, tokens) -> ident, tokens
    | Some (token, _) -> raise (Error (Unexpected_token { found = token }))
    | None -> raise (Error Unexpected_eof)

  let rec parse_rule (tokens : Lexer.t) : Rule.t * Lexer.t =
    let tokens =
      match Seq.uncons tokens with
      | Some (PIPE, tokens) -> tokens
      | _ -> tokens
    in
    let rule, tokens = parse_seq_rule tokens in
    let rules, tokens =
      let rec go (acc : Rule.t list) (tokens : Lexer.t) =
        match Seq.uncons tokens with
        | Some (PIPE, tokens) ->
            let rule, tokens = parse_seq_rule tokens in
            (go [@tailcall]) (rule :: acc) tokens
        | _ -> List.rev acc, tokens
      in
      go [] tokens
    in
    match (rule :: rules) with
    | [rule] -> rule, tokens
    | rules -> Rule.Alt rules, tokens

  and parse_seq_rule (tokens : Lexer.t) : Rule.t * Lexer.t =
    let rule, tokens = parse_rep_rule tokens in
    let rules, tokens =
      let rec go (acc : Rule.t list) (tokens : Lexer.t) =
        match Seq.uncons tokens with
        | Some ((IDENT _ | STRING _ | LEFT_PAREN), _) ->
            let rule, tokens = parse_rep_rule tokens in
            (go [@tailcall]) (rule :: acc) tokens
        | _ -> List.rev acc, tokens
      in
      go [] tokens
    in
    match (rule :: rules) with
    | [rule] -> rule, tokens
    | rules -> Rule.Seq rules, tokens

  and parse_rep_rule (tokens : Lexer.t) : Rule.t * Lexer.t =
    let rule, tokens = parse_atom_rule tokens in
    match Seq.uncons tokens with
    | Some (ASTERISK, tokens) -> Rule.Rep0 rule, tokens
    | Some (PLUS, tokens) -> Rule.Rep1 rule, tokens
    | Some (QUESTION, tokens) -> Rule.Opt rule, tokens
    | _ -> rule, tokens

  and parse_atom_rule (tokens : Lexer.t) : Rule.t * Lexer.t =
    match Seq.uncons tokens with
    | Some (IDENT ident, tokens) -> Rule.Item ident, tokens
    | Some (STRING ident, tokens) -> Rule.Token ident, tokens
    | Some (LEFT_PAREN, tokens) ->
        let rule, tokens = parse_rule tokens in
        let tokens = expect RIGHT_PAREN tokens in
        rule, tokens
    | Some (token, _) -> raise (Error (Unexpected_token { found = token }))
    | None -> raise (Error Unexpected_eof)

  let parse_item (tokens : Lexer.t) : (string * Rule.t) * Lexer.t =
    let tokens = expect KW_RULE tokens in
    let ident, tokens = expect_ident tokens in
    let tokens = expect COLON_EQUALS tokens in
    let rule, tokens = parse_rule tokens in
    (ident, rule), tokens

  let parse_grammar (tokens : Lexer.t) : Grammar.t =
    let[@tail_mod_cons] rec parse_items tokens =
      match Seq.uncons tokens with
      | Some (_, _) ->
          let item, tokens = parse_item tokens in
          item :: parse_items tokens
      | None -> []
    in
    Grammar.{ items = parse_items tokens }

end

(** Top-down, backtracking recogniser for grammars *)
module Recogniser (G : sig

  module Token : sig
    type t
    val ident : t -> string
  end

  val entrypoint : string
  val grammar : Grammar.t

end) = struct

  exception Error

  let rec recognise_rule (rule : Rule.t) (tokens : G.Token.t Seq.t) : G.Token.t Seq.t =
    match rule with
    | Item name -> recognise_item name tokens
    | Token ident ->
        begin match Seq.uncons tokens with
        | Some (token, tokens) when G.Token.ident token = ident -> tokens
        | Some _ | None -> raise Error
        end
    | Seq rules ->
        let rec go rules tokens =
          match rules with
          | [] -> tokens
          | rule :: rules ->
              let tokens = recognise_rule rule tokens in
              go rules tokens
        in
        go rules tokens
    | Alt rules ->
        let rec go rules =
          match rules with
          | [] -> raise Error
          | rule :: rules ->
              try recognise_rule rule tokens with
              | Error -> go rules
        in
        go rules
    | Opt rule ->
        begin try recognise_rule rule tokens with
        | Error -> tokens
        end
    | Rep0 rule ->
        let rec go tokens =
          match recognise_rule rule tokens with
          | tokens -> go tokens
          | exception Error -> tokens
        in
        go tokens
    | Rep1 rule ->
        begin match recognise_rule rule tokens with
        | tokens -> recognise_rule (Rep0 rule) tokens
        | exception Error -> tokens
        end

  and recognise_item (name : string) (tokens : G.Token.t Seq.t) : G.Token.t Seq.t =
    recognise_rule (List.assoc name G.grammar.items) tokens

  let recognise_grammar (tokens : G.Token.t Seq.t) =
    match recognise_item G.entrypoint tokens with
    | tokens when Seq.is_empty tokens -> ()
    | _ -> raise Error

end

module Examples = struct

  (** Grammar of grammar specifications *)
  let grammar = {|

    rule grammar    := item*
    rule item       := "rule" "IDENT" ":=" alt_rule

    rule alt_rule   := "|"? seq_rule ("|" seq_rule)*
    rule seq_rule   := rep_rule+
    rule rep_rule   := atom_rule ("*" | "+" | "?")?
    rule atom_rule  := "IDENT" | "STRING" | "(" alt_rule ")"

  |}

  (** Arithmetic expressions *)
  let arith = {|

    rule expr :=
      | add_expr ("=" | "<" | "<=" | ">" | ">=") add_expr

    rule add_expr :=
      | mul_expr (("+" | "-") mul_expr)?

    rule mul_expr :=
      | prefix_expr (("*" | "/") prefix_expr)*

    rule prefix_expr :=
      | ("+" | "-") atom_expr

    rule atom_expr :=
      | "IDENT"
      | "NUMBER"
      | "(" expr ")"

  |}

  (** Wirth’s PL/0 language from “Algorithms + Data Structures = Programs”.

      - https://en.wikipedia.org/wiki/PL/0#Grammar
      - https://en.wikipedia.org/wiki/Recursive_descent_parser#Example_parser
  *)
  let pl0 = {|

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

let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  let grammar = Lexer.tokenise Examples.grammar |> Parser.parse_grammar in
  (* NOTE: to test these we'll need a nice way to implement lexers *)
  let _ = Lexer.tokenise Examples.arith |> Parser.parse_grammar in
  let _ = Lexer.tokenise Examples.pl0 |> Parser.parse_grammar in

  let module Grammar_recogniser =
    Recogniser (struct
      module Token = Token
      let entrypoint = "grammar"
      let grammar = grammar
    end)
  in

  Lexer.tokenise Examples.grammar |> Grammar_recogniser.recognise_grammar;
  Lexer.tokenise Examples.arith |> Grammar_recogniser.recognise_grammar;
  Lexer.tokenise Examples.pl0 |> Grammar_recogniser.recognise_grammar;

  print_string " ok!\n";

end
