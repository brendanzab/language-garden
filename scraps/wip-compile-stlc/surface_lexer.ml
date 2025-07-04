exception Error of string

let unexpected_char () = raise (Error "unexpected character")
let unclosed_block_comment () = raise (Error "unclosed block comment")

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let digits = [%sedlex.regexp? Plus ('0'..'9')]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec token (lexbuf : Sedlexing.lexbuf) : Surface_parser.token =
  match%sedlex lexbuf with
  | whitespace    -> token lexbuf
  | "--"          -> line_comment lexbuf
  | "/-"          -> block_comment lexbuf 0
  | digits        -> NUMBER (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | "else"        -> KEYWORD_ELSE
  | "false"       -> KEYWORD_FALSE
  | "fun"         -> KEYWORD_FUN
  | "if"          -> KEYWORD_IF
  | "let"         -> KEYWORD_LET
  | "then"        -> KEYWORD_THEN
  | "true"        -> KEYWORD_TRUE
  | name          -> NAME (Sedlexing.Utf8.lexeme lexbuf)
  | "+"           -> ADD
  | "&&"          -> AMP_AMP
  | "*"           -> ASTERISK
  | "!"           -> BANG
  | ":"           -> COLON
  | ":="          -> COLON_EQUALS
  | "="           -> EQUALS
  | "=>"          -> EQUALS_GREATER
  | "-"           -> HYPHEN
  | "->"          -> HYPHEN_GREATER
  | "||"          -> PIPE_PIPE
  | ";"           -> SEMICOLON
  | "("           -> OPEN_PAREN
  | ")"           -> CLOSE_PAREN
  | eof           -> END
  | _             -> unexpected_char ()

and line_comment (lexbuf : Sedlexing.lexbuf) : Surface_parser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> unexpected_char ()

and block_comment (lexbuf : Sedlexing.lexbuf) (level : int) : Surface_parser.token =
  match%sedlex lexbuf with
  | "/-"          -> block_comment lexbuf (level + 1)
  | "-/"          -> if level = 0 then token lexbuf else block_comment lexbuf (level - 1)
  | any           -> block_comment lexbuf level
  | eof           -> unclosed_block_comment ()
  | _             -> unexpected_char ()
