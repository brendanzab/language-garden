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
| "fun"         { KEYWORD_FUN }
| "let"         { KEYWORD_LET }
| name as n     { NAME n }
| "+"           { ADD }
| "*"           { ASTERISK }
| ":="          { COLON_EQUALS }
| "=>"          { EQUALS_GREATER }
| "-"           { HYPHEN }
| ";"           { SEMICOLON }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }
