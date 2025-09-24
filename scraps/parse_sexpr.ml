(** Recursive descent parser for symbolic expressions

    - {{: https://en.wikipedia.org/wiki/S-expression} S-expression} on Wikipedia
*)

module Token = struct

  type t =
    | Ident of string
    | Int of int
    | Left_paren
    | Right_paren

end

module Lexer : sig

  type t = Token.t Seq.t

  exception Unexpected_char of { found : char }

  val tokenise : string -> t

end  = struct

  type t = Token.t Seq.t

  let is_digit (ch : char) : bool =
    match ch with
    | '0' .. '9' -> true
    | _ -> false

  let is_ident_start (ch : char) : bool =
    match ch with
    (* https://www.ietf.org/archive/id/draft-rivest-sexp-01.html#name-token-representation *)
    | 'a' .. 'z' | 'A' .. 'Z' | '-' | '.' | '/' | ':' | '*' | '+' | '=' -> true
    | _ -> false

  let is_ident_continue (ch : char) : bool =
    is_ident_start ch || is_digit ch

  let is_ascii_whitespace (ch : char) : bool =
    match ch with
    | ' ' | '\n' | '\r' | '\t' -> true
    | _ -> false

  exception Unexpected_char of { found : char }

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
    | Some ('(', input) -> Seq.Cons (Token.Left_paren, tokens input)
    | Some (')', input) -> Seq.Cons (Token.Right_paren, tokens input)
    | Some(ch, _) when is_digit ch ->
        let (s, input) = take_while is_digit input in
        Seq.Cons (Token.Int (int_of_string s), tokens input)
    | Some(ch, _) when is_ident_start ch ->
        let (s, input) = take_while is_ident_continue input in
        Seq.Cons (Token.Ident s, tokens input)
    | Some (ch, input) when is_ascii_whitespace ch -> tokens input ()
    | Some (ch, _) -> raise (Unexpected_char { found = ch })
    | None -> Seq.Nil

  let tokenise (input : string) : Token.t Seq.t =
    tokens (String.to_seq input)

end

module Sexpr = struct

  type t =
    | Ident of string
    | Int of int
    | List of t list

end

module Parser = struct

  exception Unexpected_token of { found : Token.t }
  exception Unexpected_eof
  exception Unconsumed_tokens of { remaining : Token.t Seq.t }

  let rec parse_sexpr (tokens : Token.t Seq.t) : Sexpr.t * Token.t Seq.t =
    match Seq.uncons tokens with
    | Some (Left_paren, tokens) -> parse_list tokens []
    | Some (Ident s, tokens) -> (Ident s, tokens)
    | Some (Int s, tokens) -> (Int s, tokens)
    | Some (token, _) -> raise (Unexpected_token { found = token })
    | None -> raise Unexpected_eof

  and parse_list (tokens : Token.t Seq.t) (acc : Sexpr.t list) : Sexpr.t * Token.t Seq.t =
    match Seq.uncons tokens with
    | Some (Right_paren, tokens) ->
        (List (List.rev acc), tokens)
    | Some _ | None ->
        let (sexpr, tokens) = parse_sexpr tokens in
        (parse_list [@tailcall]) tokens (sexpr :: acc)

  let parse (tokens : Token.t Seq.t) : Sexpr.t =
    let (sexpr, tokens) = parse_sexpr tokens in
    if Seq.is_empty tokens then sexpr else
      raise (Unconsumed_tokens { remaining = tokens })

end

let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  assert (Lexer.tokenise "()" |> List.of_seq = [Left_paren; Right_paren]);
  assert (Lexer.tokenise "foo" |> List.of_seq = [Ident "foo"]);

  assert (Parser.parse (Lexer.tokenise "()") = List []);
  assert (Parser.parse (Lexer.tokenise "foo") = Ident "foo");
  assert (Parser.parse (Lexer.tokenise "(foo)") = List [Ident "foo"]);
  assert (Parser.parse (Lexer.tokenise "(foo bar)") = List [Ident "foo"; Ident "bar"]);
  assert (Parser.parse (Lexer.tokenise "(+ 1 2 (* 3 4))") = List [Ident "+"; Int 1; Int 2; List [Ident "*"; Int 3; Int 4]]);
  assert (Parser.parse (Lexer.tokenise "(+ (* 1 2) 3 4)") = List [Ident "+"; List [Ident "*"; Int 1; Int 2]; Int 3; Int 4]);

  print_string " ok!\n";

end
