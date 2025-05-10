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

let elab_tm (tm : Surface.tm) : Core.tm * Core.Semantics.vty =
  try Surface.elab_infer Surface.empty tm with
  | Surface.Error (pos, msg) ->
      print_error pos msg;
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let tm, vty = elab_tm (parse_tm "<input>" stdin) in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) tm
    (Core.pp_ty []) (Core.Semantics.quote_vty 0 vty)

let norm_cmd () : unit =
  let tm, vty = elab_tm (parse_tm "<input>" stdin) in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) (Core.Semantics.normalise_tm [] [] tm)
    (Core.pp_ty []) (Core.Semantics.quote_vty 0 vty)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  Cmd.group (Cmd.info Sys.argv.(0)) [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a term from standard input")
      Term.(const elab_cmd $ const ());
    Cmd.v (Cmd.info "norm" ~doc:"elaborate and normalise a term from standard input")
      Term.(const norm_cmd $ const ());
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
