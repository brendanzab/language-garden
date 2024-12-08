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

let fun_lit (name, ty) body = Core.(Fun_lit (name, ty, body (Var name)))

let rec concat = function
  | [] -> Core.Text_lit ""
  | [t] -> t
  | t :: ts -> Core.Prim_app (Test_concat, [t; concat ts])

let node name body =
  concat [
    Text_lit (Format.sprintf "<%s>" name);
    body;
    Text_lit (Format.sprintf "</%s>" name);
  ]

let () = begin

  define "true" Core.Bool_ty Core.(Bool_lit true);
  define "false" Core.Bool_ty Core.(Bool_lit false);

  define "heading1" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "h1"));
  define "heading2" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "h2"));
  define "heading3" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "h3"));
  define "para" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "p"));
  define "link" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "a"));

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
  | Core.Semantics.Text_lit s -> print_string s
  | _ -> failwith "text literal expected"
  | exception Lexer.Error error ->
      let msg =
        match error with
        | `Unexpected_char -> "unexpected character"
        | `Unclosed_block_comment -> "unclosed block comment"
        | `Unclosed_text_literal -> "unclosed text literal"
        | `Invalid_escape_code s -> Format.sprintf "invalid escape code `\\%s`" s
      in
      print_error (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | exception Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1
  | exception Surface.Elab.Error (pos, msg) ->
      print_error pos msg;
      exit 1
