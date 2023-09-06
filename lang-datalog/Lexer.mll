{
  open Parser

  exception Error
}

let whitespace = [' ' '\t' '\n']
let comment = "%" [^ '\n' ]* '\n'
let digits = ['0'-'9']+
let string = "\"" [^ '"']* "\""
let lower_name = ['a'-'z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*
let upper_name = ['A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace      { token lexbuf }
| comment         { token lexbuf }
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
