(** {0 Elaborator CLI} *)

let print_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    severity
    message


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "<input>";

  match
    lexbuf
    |> Sedlexing.with_tokenizer Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.main
    |> Surface.elab_synth
  with
  | tm, ty ->
      Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
        Core.pp_tm tm
        Core.pp_ty ty
  | exception Lexer.Error error ->
      let msg =
        match error with
        | `UnexpectedChar -> "unexpected character"
        | `UnclosedBlockComment -> "unclosed block comment"
      in
      print_error "error" (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | exception Parser.Error ->
      print_error "error" (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1
  | exception Surface.Error (loc, msg) ->
      print_error "error" loc msg;
      exit 1
  | exception Surface.Bug (loc, msg) ->
      print_error "bug" loc msg;
      exit 1
