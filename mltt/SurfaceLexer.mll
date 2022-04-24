{
  open SurfaceParser

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
| "fun"         { FUN }
| "let"         { LET }
| "Type"        { TYPE }
| ident as n    { NAME n }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "->"          { HYPHEN_GREATER }
| "."           { FULL_STOP }
| ";"           { SEMICOLON }
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| eof           { END }
| _             { raise Error }
