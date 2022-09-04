{
  open Parser

  exception Error
}

let newline = '\n'
let whitespace = [' ' '\t']
let comment = "--" [^ '\n']* newline
let ident = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| comment       { Lexing.new_line lexbuf; token lexbuf }
| "_"           { UNDERSCORE }
| "fun"         { KEYWORD_FUN }
| "let"         { KEYWORD_LET }
| "Type"        { KEYWORD_TYPE }
| ident as n    { NAME n }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "->"          { HYPHEN_GREATER }
| ";"           { SEMICOLON }
| '('           { LPAREN }
| ')'           { RPAREN }
| eof           { END }
| _             { raise Error }
