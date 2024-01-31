{
  open Parser

  exception Error of [
    | `UnexpectedChar
    | `UnclosedBlockComment
  ]
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digits = ['0'-'9']+
let name = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
| whitespace    { token lexbuf }
| newline       { Lexing.new_line lexbuf; token lexbuf }
| "--"          { line_comment lexbuf }
| "/-"          { block_comment 0 lexbuf }
| digits as n   { NUMBER (int_of_string n) }
| "else"        { KEYWORD_ELSE }
| "end"         { KEYWORD_END }
| "false"       { KEYWORD_FALSE }
| "fun"         { KEYWORD_FUN }
| "if"          { KEYWORD_IF }
| "let"         { KEYWORD_LET }
| "match"       { KEYWORD_MATCH }
| "then"        { KEYWORD_THEN }
| "true"        { KEYWORD_TRUE }
| "with"        { KEYWORD_WITH }
| name as n     { NAME n }
| "+"           { ADD }
| "*"           { ASTERISK }
| ":"           { COLON }
| ":="          { COLON_EQUALS }
| "="           { EQUALS }
| "=>"          { EQUALS_GREATER }
| "-"           { HYPHEN }
| "->"          { HYPHEN_GREATER }
| ";"           { SEMICOLON }
| "_"           { UNDERSCORE }
| "|"           { VERTICAL_LINE }
| "["           { OPEN_BRACE }
| "]"           { CLOSE_BRACE }
| "("           { OPEN_PAREN }
| ")"           { CLOSE_PAREN }
| eof           { END }
| _             { raise (Error `UnexpectedChar) }

and line_comment = parse
| newline       { Lexing.new_line lexbuf; token lexbuf }
| eof           { END }
| _             { line_comment lexbuf }

and block_comment level = parse
| newline       { Lexing.new_line lexbuf; block_comment level lexbuf }
| "/-"          { block_comment (level + 1) lexbuf  }
| "-/"          { if level = 0 then token lexbuf else block_comment (level - 1) lexbuf }
| eof           { raise (Error `UnclosedBlockComment) }
| _             { block_comment level lexbuf }
