(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    severity
    message

let parse_tm (filename : string) (input : in_channel) : Surface.tm =
  let lexbuf = Sedlexing.Utf8.from_channel input in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.main
  with
  | Lexer.Error error ->
      let msg =
        match error with
        | `UnexpectedChar -> "unexpected character"
        | `UnclosedBlockComment -> "unclosed block comment"
      in
      print_error "error" (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | Parser.Error ->
      print_error "error" (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1

let elab_tm (tm : Surface.tm) : Core.tm * Core.ty =
  try Surface.elab_synth tm with
  | Surface.Error (loc, msg) ->
      print_error "error" loc msg;
      exit 1
  | Surface.Bug (loc, msg) ->
      print_error "bug" loc msg;
      exit 1


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let tm = parse_tm "<input>" stdin in
  let tm, ty = elab_tm tm in

  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    Core.pp_tm tm
    Core.pp_ty ty
