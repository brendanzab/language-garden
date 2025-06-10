(** {0 Elaborator CLI} *)

module Source_file = struct

  type t = {
    name : string;
    contents : string;
    lines : (int * int) Dynarray.t;
  }

  let create (name : string) (contents : string) : t =
    let lines = Dynarray.create () in
    let add_line stop =
      match Dynarray.find_last lines with
      | None -> Dynarray.add_last lines (0, stop)
      | Some (_, prev_stop) -> Dynarray.add_last lines (prev_stop + 1, stop)
    in
    contents |> String.iteri (fun pos ch -> if ch = '\n' then add_line pos);
    add_line (String.length contents);

    { name; contents; lines }

  let get_line (source : t) (line : int) : string =
    let start, stop = Dynarray.get source.lines (line - 1) in
    String.sub source.contents start (stop - start)

end

let emit (source : Source_file.t) (severity : string) (start, stop : Surface.loc) (message : string) =
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "%s: %s\n" severity message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline

let elab_tm (source : Source_file.t) : Core.tm * Core.ty =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lex_error msg = Surface.Diagnostic.error (Sedlexing.lexing_positions lexbuf) msg in
  Sedlexing.set_filename lexbuf source.name;

  let opt_tm, diagnostics =
    match
      MenhirLib.Convert.Simplified.traditional2revised Parser.main
        (Sedlexing.with_tokenizer Lexer.token lexbuf)
    with
    | tm -> Surface.Elab.infer_tm tm
    | exception Lexer.Error message -> None, [lex_error message]
    | exception Parser.Error -> None, [lex_error "syntax error"]
  in

  let num_warnings = ref 0 in
  let num_errors = ref 0 in

  diagnostics |> List.iter begin fun (d : Surface.Diagnostic.t) ->
    match d.severity with
    | Warning -> emit source "warning" d.loc d.message; incr num_warnings
    | Error -> emit source "error" d.loc d.message; incr num_errors
  end;

  let has_warnings = !num_warnings > 0 in
  let has_errors = !num_errors > 0 in

  let emit_severity_counts severity count =
    let pluralised = if count = 1 then severity else severity ^ "s" in
    Printf.eprintf "%s: %s generated %i %s\n" severity source.name count pluralised
  in

  if has_warnings || has_errors then Printf.eprintf "\n";
  if has_warnings then emit_severity_counts "warning" !num_warnings;
  if has_errors then emit_severity_counts "error" !num_errors;
  if has_errors then exit 1;

  Option.get opt_tm


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = elab_tm source in
  Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
    (Core.pp_tm [] tm)
    (Core.pp_ty ty)

let norm_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = elab_tm source in
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
