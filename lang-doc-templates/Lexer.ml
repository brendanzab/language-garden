open Parser

exception Error of [
  | `UnexpectedChar
  | `UnclosedBlockComment
  | `UnclosedTextLiteral
  | `InvalidEscapeCode of string
]

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let dec_digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9' | 'A'..'F' | 'a'..'f']

let dec_number = [%sedlex.regexp? Plus dec_digit]
let hex_number = [%sedlex.regexp? "0x", Plus hex_digit]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

(* Tokenisation is made challenging as a result of Menhir not supporting parsers
  with multiple lexer modes. Instead we manage a stack of lexer modes within the
  lexer to keep track of if we are inside a term or a template. *)

type mode =
  | Term
  | Template

let create_token_lexer (initial_mode : mode) : Sedlexing.lexbuf -> token =
  let mode_stack = Stack.create () in
  let pending_tokens = Stack.create () in

  Stack.push initial_mode mode_stack;

  let rec token (lexbuf : Sedlexing.lexbuf) : token =
    match Stack.pop_opt pending_tokens with
    | Some token -> token
    | None -> begin
        match Stack.top_opt mode_stack with
        | Some Term -> term_token lexbuf
        | Some Template -> template_token lexbuf
        | None -> END
    end

  and term_token (lexbuf : Sedlexing.lexbuf) : token =
    match%sedlex lexbuf with
    | whitespace -> (token [@tailcall]) lexbuf
    | "--" -> line_comment lexbuf
    | "/-" -> block_comment lexbuf 0
    | '"' -> text lexbuf
    | "$\"" ->  Stack.push Template mode_stack; OPEN_TEMPLATE
    | dec_number -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | hex_number -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | "else" -> KEYWORD_ELSE
    | "if" -> KEYWORD_IF
    | "let" -> KEYWORD_LET
    | "then" -> KEYWORD_THEN
    | name -> NAME (Sedlexing.Utf8.lexeme lexbuf)
    | "+" -> ADD
    | ":" -> COLON
    | ":=" -> COLON_EQUALS
    | "->" -> HYPHEN_GREATER
    | ";" -> SEMI
    | '(' -> OPEN_PAREN
    | ')' -> CLOSE_PAREN
    | '}' -> Stack.drop mode_stack; CLOSE_TERM
    | eof -> Stack.drop mode_stack; END
    | _ -> raise (Error `UnexpectedChar)

  and template_token (lexbuf : Sedlexing.lexbuf) : token =
    let buf = Buffer.create 1 in
    let rec go () =
      match%sedlex lexbuf with
      | "\\" -> begin
          match%sedlex lexbuf with
          | "\\" -> Buffer.add_string buf "\\"; (go [@tailcall]) ()
          | "\"" -> Buffer.add_string buf "\""; (go [@tailcall]) ()
          | "n" -> Buffer.add_string buf "\n"; (go [@tailcall]) ()
          | "t" -> Buffer.add_string buf "\t"; (go [@tailcall]) ()
          | "$" -> Buffer.add_string buf "$"; (go [@tailcall]) ()
          | _ -> raise (Error (`InvalidEscapeCode (Sedlexing.Utf8.lexeme lexbuf)))
      end
      | "$" -> begin
          match%sedlex lexbuf with
          | "{" ->
              Stack.push Term mode_stack;
              Stack.push OPEN_TERM pending_tokens;
              TEMPLATE_TEXT (Buffer.contents buf)
          | _ -> raise (Error `UnexpectedChar)
      end
      | "\"" ->
          Stack.drop mode_stack;
          Stack.push CLOSE_TEMPLATE pending_tokens;
          TEMPLATE_TEXT (Buffer.contents buf)
      (* TODO: Markdown style elements

        For example:

        - headings
        - list items
        - links
        - inline elements

        We could possibly defer this to elaboration time, however.
      *)
      | eof ->
          Stack.drop mode_stack;
          Stack.push END pending_tokens;
          TEMPLATE_TEXT (Buffer.contents buf)
      | any -> Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf); (go [@tailcall]) ()
      | _ -> raise (Error `UnexpectedChar)
    in
    go ()

  and line_comment (lexbuf : Sedlexing.lexbuf) : Parser.token =
    match%sedlex lexbuf with
    | newline -> (token [@tailcall]) lexbuf
    | any -> (line_comment [@tailcall]) lexbuf
    | eof -> END
    | _ -> raise (Error `UnexpectedChar)

  and block_comment (lexbuf : Sedlexing.lexbuf) (level : int) : token =
    match%sedlex lexbuf with
    | "/-" -> (block_comment [@tailcall]) lexbuf (level + 1)
    | "-/" -> if level = 0 then (token [@tailcall]) lexbuf else (block_comment [@tailcall]) lexbuf (level - 1)
    | any -> (block_comment [@tailcall]) lexbuf level
    | eof -> raise (Error `UnclosedBlockComment)
    | _ -> raise (Error `UnexpectedChar)

  and text (lexbuf : Sedlexing.lexbuf) : token =
    let buf = Buffer.create 1 in
    let rec go () =
      match%sedlex lexbuf with
      | "\\" -> begin
          match%sedlex lexbuf with
          | "\\" -> Buffer.add_string buf "\\"; (go [@tailcall]) ()
          | "\"" -> Buffer.add_string buf "\""; (go [@tailcall]) ()
          | "n" -> Buffer.add_string buf "\n"; (go [@tailcall]) ()
          | "t" -> Buffer.add_string buf "\t"; (go [@tailcall]) ()
          | _ -> raise (Error (`InvalidEscapeCode (Sedlexing.Utf8.lexeme lexbuf)))
      end
      | '"' -> Buffer.contents buf
      | any -> Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf); (go [@tailcall]) ()
      | eof -> raise (Error `UnclosedTextLiteral)
      | _ -> raise (Error `UnexpectedChar)
    in
    TEXT (go ())
  in

  token

let term_token () : Sedlexing.lexbuf -> token =
  create_token_lexer Term

let template_token () : Sedlexing.lexbuf -> token =
  create_token_lexer Template
