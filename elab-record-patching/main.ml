(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (start, _ : Lexing.position * Lexing.position) message =
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

let infer ctx tm =
  try Surface.infer ctx tm with
  | Surface.Error message ->
      Printf.eprintf "error: %s\n" message;
      exit 1

let pp_def ~resugar ctx ppf (name, ty, tm) =
  let pp_tm = Surface.pp ~resugar ctx in
  let pp_name_ann ppf (name, ty) =
    Format.fprintf ppf "@[<2>@[%s :@]@ @[%a@]@]" name pp_tm ty
  in
  Format.fprintf ppf "@[<2>@[%a@ :=@]@ @[%a@]@]"
    pp_name_ann (name, ty)
    (Surface.pp ~resugar ctx) tm


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let ctx = Surface.initial_context in
  let (tm, ty) = infer ctx (parse_tm "<input>" stdin) in
  Format.printf "%a@\n" (pp_def ~resugar:false ctx)
    ("<input>", Surface.quote ctx ty, tm)

let norm_cmd () : unit =
  let ctx = Surface.initial_context in
  let (tm, ty) = infer ctx (parse_tm "<input>" stdin) in
  Format.printf "%a@\n" (pp_def ~resugar:true ctx)
    ("<input>", Surface.quote ctx ty, Surface.normalise ctx tm)


(** {1 CLI options} *)

let cmd : unit Cmdliner.Cmd.t =
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
