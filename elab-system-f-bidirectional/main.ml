(** {0 Elaborator CLI} *)

module Source_file = struct

  type t = {
    name : string;
    contents : string;
    lines : (int * int) Dynarray.t
  }

  let from_channel (name : string) (ch : in_channel) : t =
    let buf = Buffer.create 16 in
    let lines = Dynarray.create () in

    let rec loop () =
      try
        let line = input_line ch in
        if not (Dynarray.is_empty lines) then Buffer.add_char buf '\n';
        Dynarray.add_last lines (Buffer.length buf, String.length line);
        Buffer.add_string buf line;
        loop ()
      with
      | End_of_file ->
          let contents = Buffer.contents buf in
          { name; contents; lines }
    in

    loop ()

  let get_line (source : t) (line : int) : string =
    let pos, len = Dynarray.get source.lines (line - 1) in
    String.sub source.contents pos len

end

let print_error (source : Source_file.t) (start, stop : Surface.loc) (message : string) =
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "error: %s\n" message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline

let parse_tm (source : Source_file.t) : Surface.tm =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf source.name;

  try
    MenhirLib.Convert.Simplified.traditional2revised Parser.main
      (Sedlexing.with_tokenizer Lexer.token lexbuf)
  with
  | Lexer.Error `Unexpected_char -> print_error source (lexpos ()) "unexpected character"; exit 1
  | Lexer.Error `Unclosed_block_comment -> print_error source (lexpos ()) "unclosed block comment"; exit 1
  | Parser.Error -> print_error source (lexpos ()) "syntax error"; exit 1

let elab_tm (source : Source_file.t) (tm : Surface.tm) : Core.tm * Core.Semantics.vty =
  try Surface.Elab.infer_tm Surface.Elab.empty tm with
  | Surface.Elab.Error (pos, msg) ->
      print_error source pos msg;
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let source = Source_file.from_channel "<stdin>" stdin in
  let tm, vty = parse_tm source |> elab_tm source in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) tm
    (Core.pp_ty []) (Core.Semantics.quote_vty 0 vty)

let norm_cmd () : unit =
  let source = Source_file.from_channel "<stdin>" stdin in
  let tm, vty = parse_tm source |> elab_tm source in
  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm [] []) (Core.Semantics.normalise_tm [] [] tm)
    (Core.pp_ty []) (Core.Semantics.quote_vty 0 vty)


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
