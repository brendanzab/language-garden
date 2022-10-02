{
  open TreeLangParser

  exception Error
}

let whitespace = [' ' '\t' '\n']
let comment = "#" [^ '\n' ]* '\n'
let digit = ['0'-'9']

rule token = parse
| whitespace    { token lexbuf }
| comment       { token lexbuf }
| digit+ as n   { NUMBER (int_of_string n) }
| "else"        { KEYWORD_ELSE }
| "false"       { KEYWORD_FALSE }
| "if"          { KEYWORD_IF }
| "then"        { KEYWORD_THEN }
| "true"        { KEYWORD_TRUE }
| "+"           { ADD }
| "*"           { ASTERISK }
| "="           { EQUALS }
| "/"           { FORWARD_SLASH }
| "-"           { HYPHEN }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise Error }
