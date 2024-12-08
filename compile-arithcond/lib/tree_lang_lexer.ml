exception Error

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let digits = [%sedlex.regexp? Plus ('0'..'9')]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec token (lexbuf : Sedlexing.lexbuf) : Tree_lang_parser.token =
  match%sedlex lexbuf with
  | whitespace    -> token lexbuf
  | "#"           -> line_comment lexbuf
  | digits        -> NUMBER (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | "else"        -> KEYWORD_ELSE
  | "false"       -> KEYWORD_FALSE
  | "if"          -> KEYWORD_IF
  | "let"         -> KEYWORD_LET
  | "then"        -> KEYWORD_THEN
  | "true"        -> KEYWORD_TRUE
  | name          -> NAME (Sedlexing.Utf8.lexeme lexbuf)
  | "+"           -> ADD
  | "*"           -> ASTERISK
  | ":="          -> COLON_EQUALS
  | "="           -> EQUALS
  | "/"           -> FORWARD_SLASH
  | "-"           -> HYPHEN
  | ";"           -> SEMICOLON
  | "("           -> OPEN_PAREN
  | ")"           -> CLOSE_PAREN
  | eof           -> END
  | _             -> raise Error

and line_comment (lexbuf : Sedlexing.lexbuf) : Tree_lang_parser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> raise Error
