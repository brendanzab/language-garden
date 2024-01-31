{
  open TreeLangParser

  exception Error
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digits = ['0'-'9']+
let name = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| "#"           { line_comment lexbuf }
| digits as n   { NUMBER (int_of_string n) }
| "else"        { KEYWORD_ELSE }
| "false"       { KEYWORD_FALSE }
| "if"          { KEYWORD_IF }
| "let"         { KEYWORD_LET }
| "then"        { KEYWORD_THEN }
| "true"        { KEYWORD_TRUE }
| name as n     { NAME n }
| "+"           { ADD }
| "*"           { ASTERISK }
| ":="          { COLON_EQUALS }
| "="           { EQUALS }
| "/"           { FORWARD_SLASH }
| "-"           { HYPHEN }
| ";"           { SEMICOLON }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }

and line_comment = parse
| newline       { Lexing.new_line lexbuf; token lexbuf }
| eof           { END }
| _             { line_comment lexbuf }
