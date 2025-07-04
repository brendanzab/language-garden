exception Error of string

let unexpected_char () = raise (Error "unexpected character")
let unclosed_string_literal () = raise (Error "unclosed string literal")
let invalid_escape_code (lexeme : string) = raise (Error (Format.sprintf "invalid escape code `\\%s`" lexeme))

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
  | _             -> unexpected_char ()

and line_comment (lexbuf : Sedlexing.lexbuf) : Parser.token =
  match%sedlex lexbuf with
  | newline       -> token lexbuf
  | any           -> line_comment lexbuf
  | eof           -> END
  | _             -> unexpected_char ()

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
        | _ -> invalid_escape_code (Sedlexing.Utf8.lexeme lexbuf)
    end
    | "\"" -> Buffer.contents buf
    | any  -> Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf); (go [@tailcall]) ()
    | eof -> unclosed_string_literal ()
    | _ -> unexpected_char ()
  in
  STRING (go ())
