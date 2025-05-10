(** {0 Compiler CLI} *)

module Surface = Elab_builtins.Surface
module Core = Elab_builtins.Core


(** Helper functions *)

let print_error (pos : Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let parse_module filename in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;
  try
    Surface.Parser.main Surface.Lexer.token lexbuf
  with
  | Surface.Lexer.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "unexpected character";
      exit 1
  | Surface.Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "syntax error";
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let items = parse_module "<input>" stdin in
  let context = Surface.Elab.initial_context in
  let items = Surface.Elab.elab_items context items in
  items |> List.iter (function
    | Core.Syntax.Def { label; ty; tm } ->
        Format.printf "def %s : %a := %a;\n"
          label
          Core.Syntax.pp_tm ty
          Core.Syntax.pp_tm tm)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a module from standard input")
      Term.(const elab_cmd $ const ());
    (* TODO: `norm` command *)
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
