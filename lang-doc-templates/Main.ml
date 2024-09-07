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

let () = begin

  define "true" Core.BoolTy Core.(BoolLit true);
  define "false" Core.BoolTy Core.(BoolLit false);

  define "heading1" Core.(FunTy (TextTy, TextTy)) (fun_lit ("text", TextTy) (node "h1"));
  define "heading2" Core.(FunTy (TextTy, TextTy)) (fun_lit ("text", TextTy) (node "h2"));
  define "heading3" Core.(FunTy (TextTy, TextTy)) (fun_lit ("text", TextTy) (node "h3"));
  define "para" Core.(FunTy (TextTy, TextTy)) (fun_lit ("text", TextTy) (node "p"));
  define "link" Core.(FunTy (TextTy, TextTy)) (fun_lit ("text", TextTy) (node "a"));

end

let context = !context
let env = !env

let () =
  Printexc.record_backtrace true;

  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "<input>";

  match
    lexbuf
    |> Sedlexing.with_tokenizer (Lexer.template_token ())
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.template_main
    |> Surface.Elab.synth_template context
    |> Core.Semantics.eval env
  with
  | Core.Semantics.TextLit s -> print_string s
  | _ -> failwith "text literal expected"
  | exception Lexer.Error error ->
      let msg =
        match error with
        | `UnexpectedChar -> "unexpected character"
        | `UnclosedBlockComment -> "unclosed block comment"
        | `UnclosedTextLiteral -> "unclosed text literal"
        | `InvalidEscapeCode s -> Format.sprintf "invalid escape code `\\%s`" s
      in
      print_error (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | exception Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1
  | exception Surface.Elab.Error (pos, msg) ->
      print_error pos msg;
      exit 1
