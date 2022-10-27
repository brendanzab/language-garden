{
  open SurfaceParser

  exception Error
}

let newline = '\n'
let whitespace = [' ' '\t']
let comment = "--" [^ '\n']* newline
let ident = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*
let digit = ['0'-'9']
let digits = digit+

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| comment       { Lexing.new_line lexbuf; token lexbuf }
| "def"         { KEYWORD_DEF }
| "use"         { KEYWORD_USE }
| ident as n    { NAME n }
| digits as n   { NUMBER n }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "."           { FULL_STOP }
| "-"           { HYPHEN }
| "+"           { PLUS }
| ";"           { SEMICOLON }
| eof           { END }
| _             { raise Error }
