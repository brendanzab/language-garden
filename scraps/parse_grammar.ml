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
    | Keyword_def
    | Ident of string
    | Quoted of string
    | Asterisk
    | Colon_equals
    | Pipe
    | Plus
    | Question
    | Left_paren
    | Right_paren

  let ident (token : t) : string =
    match token with
    | Keyword_def -> "def"
    | Ident _ -> "IDENT"
    | Quoted _ -> "QUOTED"
    | Asterisk -> "*"
    | Colon_equals -> ":="
    | Pipe -> "|"
    | Plus -> "+"
    | Question -> "?"
    | Left_paren -> "("
    | Right_paren -> ")"

end

module Lexer : sig

  type t = Token.t Seq.t

  exception Error of [
    | `Unexpected_char of char
    | `Unexpected_escape_code of char
    | `Unexpected_eof
  ]

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

  exception Error of [
    | `Unexpected_char of char
    | `Unexpected_escape_code of char
    | `Unexpected_eof
  ]

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

  let[@tail_mod_cons] rec tokens (input : char Seq.t) () =
    match Seq.uncons input with
    | Some ('*', input) -> Seq.Cons (Token.Asterisk, tokens input)
    | Some (':', input) ->
        begin match Seq.uncons input with
        | Some ('=', input) -> Seq.Cons (Token.Colon_equals, tokens input)
        | Some (ch, _) -> raise (Error (`Unexpected_char ch))
        | None -> raise (Error `Unexpected_eof)
        end
    | Some ('|', input) -> Seq.Cons (Token.Pipe, tokens input)
    | Some ('+', input) -> Seq.Cons (Token.Plus, tokens input)
    | Some ('?', input) -> Seq.Cons (Token.Question, tokens input)
    | Some ('(', input) -> Seq.Cons (Token.Left_paren, tokens input)
    | Some (')', input) -> Seq.Cons (Token.Right_paren, tokens input)
    | Some ('"', input) ->
        let buf = Buffer.create 16 in
        let rec go input =
          match Seq.uncons input with
          | Some ('"', input) -> input
          | Some ('\\', input) ->
              begin match Seq.uncons input with
              | Some ('\\'| '"' as ch, input) ->
                  Buffer.add_char buf ch;
                  (go [@tailcall]) input
              | Some (ch, _) -> raise (Error (`Unexpected_escape_code ch))
              | None -> raise (Error `Unexpected_eof)
              end
          | Some (ch, input) ->
              Buffer.add_char buf ch;
              (go [@tailcall]) input
          | None ->
              raise (Error `Unexpected_eof)
        in
        let input = go input in
        Seq.Cons (Token.Quoted (String.of_bytes (Buffer.to_bytes buf)), tokens input)
    | Some (ch, _) when is_ident_start ch ->
        let (ident, input) = take_while is_ident_continue input in
        begin match ident with
        | "def" -> Seq.Cons (Token.Keyword_def, tokens input)
        | ident -> Seq.Cons (Token.Ident ident, tokens input)
        end
    | Some (ch, input) when is_ascii_whitespace ch -> tokens input ()
    | Some (ch, _) -> raise (Error (`Unexpected_char ch))
  | None -> Seq.Nil

  let tokenise (input : string) : t =
    tokens (String.to_seq input)

end

module Parser : sig

  exception Error of [
    | `Unexpected_eof
    | `Unexpected_token of Token.t
  ]

  val parse_grammar : Lexer.t -> Grammar.t

end = struct

  exception Error of [
    | `Unexpected_token of Token.t
    | `Unexpected_eof
  ]

  let expect (token : Token.t) (tokens : Lexer.t) : Lexer.t =
    match Seq.uncons tokens with
    | Some (t, tokens) when t = token -> tokens
    | Some (token, _) -> raise (Error (`Unexpected_token token))
    | None -> raise (Error `Unexpected_eof)

  let expect_ident (tokens : Lexer.t) : string * Lexer.t =
    match Seq.uncons tokens with
    | Some (Ident ident, tokens) -> ident, tokens
    | Some (token, _) -> raise (Error (`Unexpected_token token))
    | None -> raise (Error `Unexpected_eof)

  let rec parse_rule (tokens : Lexer.t) : Rule.t * Lexer.t =
    let tokens =
      match Seq.uncons tokens with
      | Some (Pipe, tokens) -> tokens
      | _ -> tokens
    in
    let rule, tokens = parse_seq_rule tokens in
    let rules, tokens =
      let rec go (acc : Rule.t list) (tokens : Lexer.t) =
        match Seq.uncons tokens with
        | Some (Pipe, tokens) ->
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
        | Some ((Ident _ | Quoted _ | Left_paren), _) ->
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
    | Some (Asterisk, tokens) -> Rule.Rep0 rule, tokens
    | Some (Plus, tokens) -> Rule.Rep1 rule, tokens
    | Some (Question, tokens) -> Rule.Opt rule, tokens
    | _ -> rule, tokens

  and parse_atom_rule (tokens : Lexer.t) : Rule.t * Lexer.t =
    match Seq.uncons tokens with
    | Some (Ident ident, tokens) -> Rule.Item ident, tokens
    | Some (Quoted ident, tokens) -> Rule.Token ident, tokens
    | Some (Left_paren, tokens) ->
        let rule, tokens = parse_rule tokens in
        let tokens = expect Right_paren tokens in
        rule, tokens
    | Some (token, _) -> raise (Error (`Unexpected_token token))
    | None -> raise (Error `Unexpected_eof)

  let parse_item (tokens : Lexer.t) : (string * Rule.t) * Lexer.t =
    let tokens = expect Keyword_def tokens in
    let ident, tokens = expect_ident tokens in
    let tokens = expect Colon_equals tokens in
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

  let grammar_grammar = {|

    def grammar   := item*
    def item      := "def" "IDENT" ":=" rule

    def rule      := alt_rule
    def alt_rule  := "|"? seq_rule ("|" seq_rule)*
    def seq_rule  := rep_rule+
    def rep_rule  := atom_rule ("*" | "+" | "?")?
    def atom_rule := "IDENT" | "QUOTED" | "(" rule ")"

  |}

  (** Wirth’s PL/0 language from “Algorithms + Data Structures = Programs”.

      - https://en.wikipedia.org/wiki/PL/0#Grammar
      - https://en.wikipedia.org/wiki/Recursive_descent_parser#Example_parser
  *)
  let pl0_grammar = {|

    def program :=
      | block "."

    def block :=
      ("const" "IDENT" "=" "NUMBER" ("," "IDENT" "=" "NUMBER")* ";")?
      ("var" "IDENT" ("," "IDENT")* ";")?
      ("procedure" "IDENT" ";" block ";")* statement

    def statement :=
      | "IDENT" ":=" expression
      | "call" "IDENT"
      | "begin" statement (";" statement)* "end"
      | "if" condition "then" statement
      | "while" condition "do" statement

    def condition :=
      | "odd" expression
      | expression ("=" | "#" | "<" | "<=" | ">" | ">=") expression

    def expression :=
      | ("+" | "-")? term (("+" | "-") term)?

    def term :=
      | factor (("*" | "/") factor)*

    def factor :=
      | "IDENT"
      | "NUMBER"
      | "(" expression ")"

  |}

end

let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  let grammar = Examples.grammar_grammar |> Lexer.tokenise |> Parser.parse_grammar in
  let _ = Examples.pl0_grammar |> Lexer.tokenise |> Parser.parse_grammar in

  let module Grammar_recogniser =
    Recogniser (struct
      module Token = Token
      let entrypoint = "grammar"
      let grammar = grammar
    end)
  in

  Examples.grammar_grammar |> Lexer.tokenise |> Grammar_recogniser.recognise_grammar;
  Examples.pl0_grammar |> Lexer.tokenise |> Grammar_recogniser.recognise_grammar;

  print_string " ok!\n";

end
