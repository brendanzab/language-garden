{
  open Parser

  exception Error
}

let whitespace = [' ' '\t' '\n']
let comment = "--" [^ '\n' ]* '\n'
let digit = ['0'-'9']

rule token = parse
| whitespace    { token lexbuf }
| comment       { token lexbuf }
| digit+ as n   { NUMBER (int_of_string n) }
| "+"           { ADD }
| "-"           { HYPHEN }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }
