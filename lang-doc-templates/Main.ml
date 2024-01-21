let print_error (start, _ : Surface.loc) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      message

let context = ref []
let env = ref []

let define name ty term =
  context := (name, ty) :: !context;
  env := (name, Core.Semantics.eval !env term) :: !env

let fun_lit (name, ty) body = Core.(FunLit (name, ty, body (Var name)))

let rec concat = function
  | [] -> Core.TextLit ""
  | [t] -> t
  | t :: ts -> Core.TextConcat (t, concat ts)

let node name body =
  Core.(concat [
    TextLit (Format.sprintf "<%s>" name);
    body;
    TextLit (Format.sprintf "</%s>" name);
  ])

let () =

  define "true" Core.Bool Core.(BoolLit true);
  define "false" Core.Bool Core.(BoolLit false);

  define "heading1" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "h1"));
  define "heading2" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "h2"));
  define "heading3" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "h3"));
  define "para" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "p"));
  define "link" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "a"));

  ()

let context = !context
let env = !env

let () =
  Printexc.record_backtrace true;

  let lexer = Lexer.template_token () in
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "<input>";

  let tm =
    try
      lexbuf
      |> Sedlexing.with_tokenizer lexer
      |> MenhirLib.Convert.Simplified.traditional2revised Parser.template_main
      |> Surface.elab_template context
    with
    | Lexer.UnexpectedChar -> print_error (Sedlexing.lexing_positions lexbuf) "unexpected character"; exit 1
    | Lexer.UnexpectedCloseUnquote -> print_error (Sedlexing.lexing_positions lexbuf) "unexpected close unquote"; exit 1
    | Lexer.UnexpectedCloseTemplate -> print_error (Sedlexing.lexing_positions lexbuf) "unexpected close template"; exit 1
    | Lexer.UnclosedBlockComment -> print_error (Sedlexing.lexing_positions lexbuf) "unclosed block comment"; exit 1
    | Lexer.UnclosedTextLiteral -> print_error (Sedlexing.lexing_positions lexbuf) "unclosed text literal"; exit 1
    | Lexer.UnclosedTemplate -> print_error (Sedlexing.lexing_positions lexbuf) "unclosed template"; exit 1
    | Lexer.InvalidEscapeCode s -> print_error (Sedlexing.lexing_positions lexbuf) ("invalid escape code `\\" ^ s ^ "`"); exit 1
    | Parser.Error -> print_error (Sedlexing.lexing_positions lexbuf) "syntax error"; exit 1
    | Surface.Error (pos, msg) -> print_error pos msg; exit 1
  in

  match Core.Semantics.eval env tm with
  | Core.Semantics.TextLit s -> print_string s
  | _ -> failwith "unexpected value"
