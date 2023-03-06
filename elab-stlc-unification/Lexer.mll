{
  open Parser

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
| "else"        { KEYWORD_ELSE }
| "false"       { KEYWORD_FALSE }
| "fun"         { KEYWORD_FUN }
| "if"          { KEYWORD_IF }
| "let"         { KEYWORD_LET }
| "then"        { KEYWORD_THEN }
| "true"        { KEYWORD_TRUE }
| name as n     { NAME n }
| "+"           { ADD }
| "*"           { ASTERISK }
| ":="          { COLON_EQUALS }
| "="           { EQUALS }
| "=>"          { EQUALS_GREATER }
| "-"           { HYPHEN }
| ";"           { SEMICOLON }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }
