{
  open FunParser

  exception Error
}

let whitespace = [' ' '\t' '\n']
let comment = "--" [^ '\n' ]* '\n'
let digits = ['0'-'9']+
let name = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| comment       { token lexbuf }
| digits as n   { NUMBER (int_of_string n) }
| "Bool"        { KEYWORD_BOOL }
| "false"       { KEYWORD_FALSE }
| "fun"         { KEYWORD_FUN }
| "Int"         { KEYWORD_INT }
| "let"         { KEYWORD_LET }
| "true"        { KEYWORD_TRUE }
| name as n     { NAME n }
| "+"           { ADD }
| "*"           { ASTERISK }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "=>"          { EQUALS_GREATER }
| "-"           { HYPHEN }
| "->"          { HYPHEN_GREATER }
| ";"           { SEMICOLON }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }
