{
  open Parser

  exception Error
}

let whitespace = [' ' '\t' '\n']
let comment = "--" [^ '\n' ]* '\n'
let name = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| comment       { token lexbuf }
| "fun"         { KEYWORD_FUN }
| "let"         { KEYWORD_LET }
| name as n     { NAME n }
| ":="          { COLON_EQUALS }
| "=>"          { EQUALS_GREATER }
| ";"           { SEMICOLON }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }
