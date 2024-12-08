exception Error of [
  | `Unexpected_char
  | `Unclosed_block_comment
]

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec token (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | whitespace    -> token lexbuf
  | "--"          -> line_comment lexbuf
  | "/-"          -> block_comment lexbuf 0
  | "_"           -> UNDERSCORE
  | "fun"         -> KEYWORD_FUN
  | "let"         -> KEYWORD_LET
  | "Type"        -> KEYWORD_TYPE
  | name          -> NAME (Sedlexing.Utf8.lexeme lexbuf)
  | ":"           -> COLON
  | ":="          -> COLON_EQUALS
  | "."           -> DOT
  | "="           -> EQUALS
  | "->"          -> HYPHEN_GREATER
  | ";"           -> SEMICOLON
  | "{"           -> LBRACE
  | "}"           -> RBRACE
  | "["           -> LBRACK
  | "]"           -> RBRACK
  | "("           -> LPAREN
  | ")"           -> RPAREN
  | eof           -> END
  | _             -> raise (Error `Unexpected_char)

and line_comment (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> raise (Error `Unexpected_char)

and block_comment (lexbuf : Sedlexing.lexbuf) (level : int) : Parser.token =
  match%sedlex lexbuf with
  | "/-"          -> block_comment lexbuf (level + 1)
  | "-/"          -> if level = 0 then token lexbuf else block_comment lexbuf (level - 1)
  | any           -> block_comment lexbuf level
  | eof           -> raise (Error `Unclosed_block_comment)
  | _             -> raise (Error `Unexpected_char)
