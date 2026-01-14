exception Error of string

let unexpected_char () = raise (Error "unexpected character")
let unclosed_block_comment () = raise (Error "unclosed block comment")

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let digits = [%sedlex.regexp? Plus ('0'..'9')]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec token (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | whitespace    -> token lexbuf
  | "--"          -> line_comment lexbuf
  | "/-"          -> block_comment lexbuf 0
  | digits        -> NUMBER (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | "else"        -> KEYWORD_ELSE
  | "fun"         -> KEYWORD_FUN
  | "if"          -> KEYWORD_IF
  | "let"         -> KEYWORD_LET
  | "then"        -> KEYWORD_THEN
  | "rec"         -> KEYWORD_REC
  | "type"        -> KEYWORD_TYPE
  | name          -> NAME (Sedlexing.Utf8.lexeme lexbuf)
  | "+"           -> ADD
  | "*"           -> ASTERISK
  | ":"           -> COLON
  | ":="          -> COLON_EQUALS
  | ","           -> COMMA
  | "."           -> DOT
  | "="           -> EQUALS
  | "=>"          -> EQUALS_GREATER
  | "-"           -> HYPHEN
  | "->"          -> HYPHEN_GREATER
  | ";"           -> SEMICOLON
  | "_"           -> UNDERSCORE
  | "["           -> OPEN_BRACKET
  | "]"           -> CLOSE_BRACKET
  | "{"           -> OPEN_BRACE
  | "}"           -> CLOSE_BRACE
  | "("           -> OPEN_PAREN
  | ")"           -> CLOSE_PAREN
  | eof           -> END
  | _             -> unexpected_char ()

and line_comment (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> unexpected_char ()

and block_comment (lexbuf : Sedlexing.lexbuf) (level : int) : Parser.token =
  match%sedlex lexbuf with
  | "/-"          -> block_comment lexbuf (level + 1)
  | "-/"          -> if level = 0 then token lexbuf else block_comment lexbuf (level - 1)
  | any           -> block_comment lexbuf level
  | eof           -> unclosed_block_comment ()
  | _             -> unexpected_char ()
