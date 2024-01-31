{
  open Parser

  exception Error
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digits = ['0'-'9']+
let string = "\"" [^ '"']* "\"" (* TODO: fix newlines *)
let lower_name = ['a'-'z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*
let upper_name = ['A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace      { token lexbuf }
| newline         { Lexing.new_line lexbuf; token lexbuf }
| "%"             { line_comment lexbuf }
| digits as n     { NUMBER (int_of_string n) }
| string as s     { STRING String.(sub s 1 (length s - 2)) }
| lower_name as n { LOWER_NAME n }
| upper_name as n { UPPER_NAME n }
| ","             { COMMA }
| "."             { DOT }
| "<-"            { GREATER_HYPHEN }
| "?"             { QUESTION }
| "("             { OPEN_PAREN }
| ")"             { CLOSE_PAREN }
| eof             { END }
| _               { raise Error }

and line_comment = parse
| newline         { Lexing.new_line lexbuf; token lexbuf }
| eof             { END }
| _               { line_comment lexbuf }
