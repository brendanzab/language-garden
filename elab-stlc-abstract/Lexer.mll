{
  open Parser

  exception Error
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| "--"          { line_comment lexbuf }
| "A"           { KEYWORD_A }
| "B"           { KEYWORD_B }
| "C"           { KEYWORD_C }
| "fun"         { KEYWORD_FUN }
| "let"         { KEYWORD_LET }
| name as n     { NAME n }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "=>"          { EQUALS_GREATER }
| "->"          { HYPHEN_GREATER }
| ";"           { SEMICOLON }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }

and line_comment = parse
| newline       { Lexing.new_line lexbuf; token lexbuf }
| eof           { END }
| _             { line_comment lexbuf }
