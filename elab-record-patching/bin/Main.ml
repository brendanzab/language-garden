(** {0 Elaborator CLI} *)

module Surface = ElabRecordPatching.Surface
module Core = ElabRecordPatching.Core
module Lexer = ElabRecordPatching.Lexer
module Parser = ElabRecordPatching.Parser

(** Helper functions *)

let print_error (start, _ : Lexing.position * Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    message

let parse_tm filename in_channel =
  let lexbuf = Sedlexing.Utf8.from_channel in_channel in
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
      print_error (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1

let infer context tm =
  try Surface.infer context tm with
  | Surface.Error message ->
      Printf.eprintf "error: %s\n" message;
      exit 1

let pp_def ~resugar context fmt (name, ty, tm) =
  let pp_tm = Surface.pp ~resugar context in
  let pp_name_ann fmt (name, ty) =
    Format.fprintf fmt "@[<2>@[%s :@]@ @[%a@]@]" name pp_tm ty
  in
  Format.fprintf fmt "@[<2>@[%a@ :=@]@ @[%a@]@]"
    pp_name_ann (name, ty)
    (Surface.pp ~resugar context) tm


(** {1 Subcommands} *)

let elab_cmd (no_resugar : bool) : unit =
  let context = Surface.initial_context in
  let (tm, ty) = infer context (parse_tm "<input>" stdin) in
  Format.printf "%a@\n" (pp_def ~resugar:(not no_resugar) context)
    ("<input>", Surface.quote context ty Core.Semantics.Univ, tm)

let norm_cmd (no_resugar : bool) : unit =
  let context = Surface.initial_context in
  let (tm, ty) = infer context (parse_tm "<input>" stdin) in
  Format.printf "%a@\n" (pp_def ~resugar:(not no_resugar) context)
    ("<input>", Surface.quote context ty Core.Semantics.Univ, Surface.normalise context tm ty)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let no_resugar : bool Term.t =
    Arg.(value & flag
      & info ["no-resugar"]
          ~doc:"disable resugaring in pretty printed terms")
  in

  Cmd.group (Cmd.info "dependent") [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a term from standard input")
      Term.(const elab_cmd $ no_resugar);
    Cmd.v (Cmd.info "norm" ~doc:"elaborate a term from standard input")
      Term.(const norm_cmd $ no_resugar);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
