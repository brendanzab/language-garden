exception Error

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let digits = [%sedlex.regexp? Plus ('0'..'9')]

let rec token (lexbuf : Sedlexing.lexbuf) : TreeLangParser.token =
  match%sedlex lexbuf with
  | whitespace    -> token lexbuf
  | "#"           -> line_comment lexbuf
  | digits        -> NUMBER (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | "+"           -> ADD
  | "*"           -> ASTERISK
  | "/"           -> FORWARD_SLASH
  | "-"           -> HYPHEN
  | "("           -> OPEN_PAREN
  | ")"           -> CLOSE_PAREN
  | eof           -> END
  | _             -> raise Error

and line_comment (lexbuf : Sedlexing.lexbuf) : TreeLangParser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> raise Error
