open Parser

exception Error of [
  | `UnexpectedChar
  | `UnexpectedCloseUnquote
  | `UnexpectedCloseTemplate
  | `UnexpectedEndOfFile
  | `UnclosedBlockComment
  | `UnclosedTextLiteral
  | `UnclosedTemplate
  | `InvalidEscapeCode of string
]

let dec_digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9' | 'A'..'F' | 'a'..'f']

let dec_number = [%sedlex.regexp? Plus dec_digit]
let hex_number = [%sedlex.regexp? "0x", Plus hex_digit]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec block_comment (lexbuf : Sedlexing.lexbuf) (level : int) : unit =
  match%sedlex lexbuf with
  | "/-" -> (block_comment [@tailcall]) lexbuf (level + 1)
  | "-/" -> if level = 0 then () else (block_comment [@tailcall]) lexbuf (level - 1)
  | any -> (block_comment [@tailcall]) lexbuf level
  | eof -> raise (Error `UnclosedBlockComment)
  | _ -> raise (Error `UnexpectedChar)

let text (lexbuf : Sedlexing.lexbuf) : string =
  let buf = Buffer.create 1 in
  let rec go () =
    match%sedlex lexbuf with
    | "\\" -> begin
        match%sedlex lexbuf with
        | "\\" -> Buffer.add_string buf "\\"; (go [@tailcall]) ()
        | "\"" -> Buffer.add_string buf "\""; (go [@tailcall]) ()
        | "n" -> Buffer.add_string buf "\n"; (go [@tailcall]) ()
        | "t" -> Buffer.add_string buf "\t"; (go [@tailcall]) ()
        | _ -> raise (Error (`InvalidEscapeCode (Sedlexing.Utf8.lexeme lexbuf)))
    end
    | '"' -> Buffer.contents buf
    | any  -> Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf); (go [@tailcall]) ()
    | eof -> raise (Error `UnclosedTextLiteral)
    | _ -> raise (Error `UnexpectedChar)
  in
  go ()

(* Tokenisation is made challenging as a result of Menhir not supporting parsers
  with multiple lexer modes. Instead we manage a stack of lexer modes within the
  lexer to keep track of if we are inside a term or a template. *)

type mode =
  | Term
  | Template

let rec token (lexbuf : Sedlexing.lexbuf) (stack : mode list ref) : token =
  (* FIXME: Surely we can clean this up somehow? *)
  match !stack with
  | Term :: stack' -> begin
      match%sedlex lexbuf with
      | ' ' | '\t' | '\n' -> (token [@tailcall]) lexbuf stack
      | "--", Star (Compl '\n'), '\n' -> (token [@tailcall]) lexbuf stack
      | "/-" -> block_comment lexbuf 0; (token [@tailcall]) lexbuf stack
      | '"' -> TEXT (text lexbuf)
      | "$\"" -> stack := Template :: !stack; OPEN_TEMPLATE
      | dec_number -> INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
      | hex_number -> INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
      | "else" -> KEYWORD_ELSE
      | "if" -> KEYWORD_IF
      | "let" -> KEYWORD_LET
      | "then" -> KEYWORD_THEN
      | name -> NAME (Sedlexing.Latin1.lexeme lexbuf)
      | "+" -> ADD
      | ":" -> COLON
      | ":=" -> COLON_EQUALS
      | "->" -> HYPHEN_GREATER
      | ";" -> SEMI
      | '(' -> OPEN_PAREN
      | ')' -> CLOSE_PAREN
      | '}' -> if stack' = [] then raise (Error `UnexpectedCloseUnquote) else (stack := stack'; (token [@tailcall]) lexbuf stack)
      | eof -> if stack' = [] then END else raise (Error `UnexpectedEndOfFile)
      | _ -> raise (Error `UnexpectedChar)
  end
  | Template :: stack' -> begin
      let buf = Buffer.create 1 in
      let rec go () =
        match%sedlex lexbuf with
        | "\\" -> begin
            match%sedlex lexbuf with
            | "\\" -> Buffer.add_string buf "\\"; (go [@tailcall]) ()
            | "\"" -> Buffer.add_string buf "\""; (go [@tailcall]) ()
            | "n" -> Buffer.add_string buf "\n"; (go [@tailcall]) ()
            | "t" -> Buffer.add_string buf "\t"; (go [@tailcall]) ()
            | "$" -> Buffer.add_string buf "$"; (go [@tailcall]) ()
            | _ -> raise (Error (`InvalidEscapeCode (Sedlexing.Utf8.lexeme lexbuf)))
        end
        | "$" -> begin
            match%sedlex lexbuf with
            | "{" -> stack := Term :: !stack; TEMPLATE_TEXT (Buffer.contents buf)
            | _ -> raise (Error `UnexpectedChar)
        end
        | "\"" -> if stack' = [] then raise (Error `UnexpectedCloseTemplate) else (stack := stack'; CLOSE_TEMPLATE)
        (* TODO: Markdown style elements

          For example:

          - headings
          - list items
          - links
          - inline elements

          We could possibly defer this to elaboration time, however.
        *)
        | eof -> if stack' = [] then (stack := stack'; TEMPLATE_TEXT (Buffer.contents buf)) else raise (Error `UnclosedTemplate)
        | any  -> Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf); (go [@tailcall]) ()
        | _ -> raise (Error `UnexpectedChar)
      in
      go ()
  end
  | [] -> END

let term_token () : Sedlexing.lexbuf -> token =
  let stack = ref [Term] in
  fun lexbuf -> token lexbuf stack

let template_token () : Sedlexing.lexbuf -> token =
  let stack = ref [Template] in
  fun lexbuf -> token lexbuf stack
