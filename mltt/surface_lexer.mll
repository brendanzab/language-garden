{
  open Surface_parser

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
| "->"          { DASH_GREATER }
| "."           { DOT }
| ";"           { SEMI }
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| eof           { END }
| _             { raise Error }
