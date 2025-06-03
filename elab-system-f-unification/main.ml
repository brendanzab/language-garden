(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let read_all (ch : in_channel) =
    let buf = Buffer.create 16 in
    let rec loop () =
      try
        Buffer.add_string buf (input_line ch);
        Buffer.add_char buf '\n';
        loop ()
      with
      | End_of_file -> Buffer.contents buf
    in
    loop ()

let print_error (start, stop : Surface.loc) (input : string) message =
  let filename = start.pos_fname in

  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  let input_lines = String.split_on_char '\n' input in (* FIXME: Windows line endings *)

  Printf.eprintf "error: %s\n" message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad filename start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (List.nth input_lines (start_line - 1));
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline

let parse_tm (filename : string) (input : string) : Surface.tm =
  let lexbuf = Sedlexing.Utf8.from_string input in
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
      print_error (Sedlexing.lexing_positions lexbuf) input msg;
      exit 1
  | Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) input "syntax error";
      exit 1

let elab_tm (input : string) (tm : Surface.tm) : Core.tm * Core.ty =
  let ctx = Surface.Elab.empty in

  let tm, vty =
    try Surface.Elab.infer_tm ctx tm with
    | Surface.Elab.Error (pos, msg) ->
        print_error pos input msg;
        exit 1
  in

  match Surface.Elab.unsolved_metas ctx with
  | [] ->
      Surface.Elab.zonk_tm ctx tm,
      Surface.Elab.zonk_ty ctx (Surface.Elab.quote_vty ctx vty)
  | unsolved_metas ->
      unsolved_metas |> List.iter (function
        | (pos, `Forall_arg) -> print_error pos input "ambiguous type argument"
        | (pos, `Fun_param) -> print_error pos input "ambiguous parameter type"
        | (pos, `Fun_body) -> print_error pos input "ambiguous return type"
        | (pos, `If_branches) -> print_error pos input "ambiguous if expression branches"
        | (pos, `Placeholder) -> print_error pos input "unsolved placeholder");
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let input = read_all stdin in
  let tm, ty = elab_tm input (parse_tm "<stdin>" input) in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) tm
    (Core.pp_ty []) ty

let norm_cmd () : unit =
  let input = read_all stdin in
  let tm, ty = elab_tm input (parse_tm "<stdin>" input) in
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
