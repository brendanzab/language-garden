{
  open TreeLangParser

  exception Error
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digits = ['0'-'9']+

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| "#"           { line_comment lexbuf }
| digits as n   { NUMBER (int_of_string n) }
| "+"           { ADD }
| "*"           { ASTERISK }
| "/"           { FORWARD_SLASH }
| "-"           { HYPHEN }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }

and line_comment = parse
| newline       { Lexing.new_line lexbuf; token lexbuf }
| eof           { END }
| _             { line_comment lexbuf }
