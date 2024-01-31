(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (start, _ : Surface.loc) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      message

let lexeme_loc lexbuf =
  Lexing.lexeme_start_p lexbuf,
  Lexing.lexeme_end_p lexbuf


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let tm =
    let lexbuf = Lexing.from_channel stdin in
    Lexing.set_filename lexbuf "<input>";

    try
      Parser.main Lexer.token lexbuf
    with
    | Lexer.Error `UnexpectedChar -> print_error (lexeme_loc lexbuf) "unexpected character"; exit 1
    | Lexer.Error `UnclosedBlockComment -> print_error (lexeme_loc lexbuf) "unclosed block comment"; exit 1
    | Parser.Error -> print_error (lexeme_loc lexbuf) "syntax error"; exit 1
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
