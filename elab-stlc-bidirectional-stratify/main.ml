(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (start, _ : Surface.loc) message =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
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
        | `Unexpected_char -> "unexpected character"
        | `Unclosed_block_comment -> "unclosed block comment"
      in
      print_error (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let infer_tm =
    try Surface.Elab.infer [] (parse_tm "<input>" stdin) with
    | Surface.Elab.Error (pos, msg) ->
        print_error pos msg;
        exit 1
  in
  match infer_tm with
  | Univ ->
      Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
        Format.pp_print_string "Type"
        Format.pp_print_string "Type 1"

  | Type t ->
      Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
        Core.pp_ty t
        Format.pp_print_string "Type"

  | Expr (e, t) ->
      Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
        (Core.pp_expr []) e
        Core.pp_ty t

let norm_cmd () : unit =
  let tm, ty =
    try Surface.Elab.infer_expr [] (parse_tm "<input>" stdin) with
    | Surface.Elab.Error (pos, msg) ->
        print_error pos msg;
        exit 1
  in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_expr []) (Core.Semantics.normalise [] tm)
    Core.pp_ty ty


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a term from standard input")
      Term.(const elab_cmd $ const ());
    Cmd.v (Cmd.info "norm" ~doc:"elaborate and normalise a term from standard input")
      Term.(const norm_cmd $ const ());
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
