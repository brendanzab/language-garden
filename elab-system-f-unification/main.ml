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

let elab_tm (tm : Surface.tm) : Core.tm * Core.ty =
  let ctx = Surface.Elab.empty in

  let tm, vty =
    try Surface.Elab.infer_tm ctx tm with
    | Surface.Elab.Error (pos, msg) ->
        print_error pos msg;
        exit 1
  in

  match Surface.Elab.unsolved_metas ctx with
  | [] ->
      Surface.Elab.zonk_tm ctx tm,
      Surface.Elab.zonk_ty ctx (Surface.Elab.quote_vty ctx vty)
  | unsolved_metas ->
      unsolved_metas |> List.iter (function
        | (pos, `Forall_arg) -> print_error pos "ambiguous type argument"
        | (pos, `Fun_param) -> print_error pos "ambiguous parameter type"
        | (pos, `Fun_body) -> print_error pos "ambiguous return type"
        | (pos, `If_branches) -> print_error pos "ambiguous if expression branches"
        | (pos, `Placeholder) -> print_error pos "unsolved placeholder");
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let tm, ty = elab_tm (parse_tm "<input>" stdin) in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) tm
    (Core.pp_ty []) ty

let norm_cmd () : unit =
  let tm, ty = elab_tm (parse_tm "<input>" stdin) in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) (Core.Semantics.normalise_tm [] [] tm)
    (Core.pp_ty []) ty


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
