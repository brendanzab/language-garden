(** {0 Elaborator CLI} *)

module Source_file = Reporting.Source_file
module Diagnostic = Reporting.Diagnostic

let elab_tm ~(source : Source_file.t) : (Core.tm * Core.ty) Reporting.Effect.t =
  fun ~report ->
    let lexbuf = Sedlexing.Utf8.from_string source.contents in
    let lex_error msg = report (Diagnostic.error (Sedlexing.lexing_positions lexbuf) msg) in
    Sedlexing.set_filename lexbuf source.name;

    match
      MenhirLib.Convert.Simplified.traditional2revised Parser.main
        (Sedlexing.with_tokenizer Lexer.token lexbuf)
    with
    | tm -> Surface.Elab.infer_tm tm ~report
    | exception Lexer.Error message -> lex_error message; Core.(Reported_error, Meta_var (fresh_meta ()))
    | exception Parser.Error -> lex_error "syntax error"; Core.(Reported_error, Meta_var (fresh_meta ()))


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = Reporting.Effect.run source (elab_tm ~source) in
  Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
    (Core.pp_tm [] tm)
    (Core.pp_ty ty)

let norm_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = Reporting.Effect.run source (elab_tm ~source) in
  Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
    (Core.pp_tm [] (Core.Semantics.normalise [] tm))
    (Core.pp_ty ty)


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
