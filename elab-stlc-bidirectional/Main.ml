(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (start, _ : Surface.loc) message =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    message


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let tm =
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    Sedlexing.set_filename lexbuf "<input>";

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
        print_error (Sedlexing.lexing_positions lexbuf) msg;
        exit 1
    | Parser.Error ->
        print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
        exit 1
  in

  let tm, ty =
    try Surface.elab_infer [] tm with
    | Surface.Error (pos, msg) ->
        print_error pos msg;
        exit 1
  in

  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm []) tm
    Core.pp_ty ty
