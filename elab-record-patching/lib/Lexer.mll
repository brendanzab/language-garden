{
  open Parser

  exception Error of [
    | `UnexpectedChar
    | `UnclosedBlockComment
  ]
}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| "--"          { line_comment lexbuf }
| "/-"          { block_comment 0 lexbuf }
| "_"           { UNDERSCORE }
| "fun"         { KEYWORD_FUN }
| "let"         { KEYWORD_LET }
| "Type"        { KEYWORD_TYPE }
| ident as n    { NAME n }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "."           { DOT }
| "="           { EQUALS }
| "->"          { HYPHEN_GREATER }
| ";"           { SEMICOLON }
| '{'           { LBRACE }
| '}'           { RBRACE }
| '['           { LBRACK }
| ']'           { RBRACK }
| '('           { LPAREN }
| ')'           { RPAREN }
| eof           { END }
| _             { raise (Error `UnexpectedChar) }

and line_comment = parse
| newline       { Lexing.new_line lexbuf; token lexbuf }
| eof           { END }
| _             { line_comment lexbuf }

and block_comment level = parse
| newline       { Lexing.new_line lexbuf; block_comment level lexbuf }
| "/-"          { block_comment (level + 1) lexbuf  }
| "-/"          { if level = 0 then token lexbuf else block_comment (level - 1) lexbuf }
| eof           { raise (Error `UnclosedBlockComment) }
| _             { block_comment level lexbuf }
