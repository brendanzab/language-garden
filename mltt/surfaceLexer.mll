{
  open SurfaceParser

  exception Error
}

let whitespace = [' ' '\t' '\n']
let ident = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| "_"           { UNDERSCORE }
| "fun"         { FUN }
| "let"         { LET }
| "Type"        { TYPE }
| ident as n    { NAME n }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "=>"          { EQUALS_GREATER }
| "->"          { HYPHEN_GREATER }
| "."           { FULL_STOP }
| ";"           { SEMICOLON }
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| eof           { END }
| _             { raise Error }
