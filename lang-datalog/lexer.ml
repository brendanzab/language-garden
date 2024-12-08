exception Error of [
  | `Unexpected_char
  | `Unclosed_string_literal
  | `Invalid_escape_code of string
]

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\r' | '\n')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let digits = [%sedlex.regexp? Plus ('0'..'9')]
let lower_name = [%sedlex.regexp? 'a'..'z', Star ('-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9')]
let upper_name = [%sedlex.regexp? 'A'..'Z', Star ('-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9')]

let rec token (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | whitespace    -> token lexbuf
  | "%"           -> line_comment lexbuf
  | digits        -> NUMBER (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | "\""          -> string lexbuf
  | lower_name    -> LOWER_NAME (Sedlexing.Utf8.lexeme lexbuf)
  | upper_name    -> UPPER_NAME (Sedlexing.Utf8.lexeme lexbuf)
  | ","           -> COMMA
  | "."           -> DOT
  | "<-"          -> GREATER_HYPHEN
  | "?"           -> QUESTION
  | "("           -> OPEN_PAREN
  | ")"           -> CLOSE_PAREN
  | eof           -> END
  | _             -> raise (Error `Unexpected_char)

and line_comment (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> raise (Error `Unexpected_char)

and string (lexbuf : Sedlexing.lexbuf) : Parser.token =
  let buf = Buffer.create 1 in
  let rec go () =
    match%sedlex lexbuf with
    | "\\" -> begin
        match%sedlex lexbuf with
        | "\\" -> Buffer.add_string buf "\\"; (go [@tailcall]) ()
        | "\"" -> Buffer.add_string buf "\""; (go [@tailcall]) ()
        | "n" -> Buffer.add_string buf "\n"; (go [@tailcall]) ()
        | "t" -> Buffer.add_string buf "\t"; (go [@tailcall]) ()
        | _ -> raise (Error (`Invalid_escape_code (Sedlexing.Utf8.lexeme lexbuf)))
    end
    | "\"" -> Buffer.contents buf
    | any  -> Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf); (go [@tailcall]) ()
    | eof -> raise (Error `Unclosed_string_literal)
    | _ -> raise (Error `Unexpected_char)
  in
  STRING (go ())
