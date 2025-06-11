(** {0 Elaborator CLI} *)

module Diagnostic = Surface.Diagnostic
module Severity = Surface.Severity

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

let emit (source : Source_file.t) (d : Diagnostic.t) =
  let start, stop = d.loc in
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "%s: %s\n" (Severity.to_string d.severity) d.message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline

let emit_diagnostic_count (source : Source_file.t) severity count =
  Printf.eprintf "%s: %s generated %s\n"
    (Severity.to_string severity)
    source.name
    (Severity.to_string ~count severity)

let emit_diagnostics (type a) (source : Source_file.t) (prog : a Surface.Elab.elab) : a =
  let warning_count = ref 0 in
  let error_count = ref 0 in

  let report (d : Diagnostic.t) =
    match d.severity with
    | Warning -> emit source d; incr warning_count
    | Error -> emit source d; incr error_count
  in

  let result = prog ~report in

  let has_warnings = !warning_count > 0 in
  let has_errors = !error_count > 0 in

  if has_warnings || has_errors then Printf.eprintf "\n";
  if has_warnings then emit_diagnostic_count source Severity.Warning !warning_count;
  if has_errors then emit_diagnostic_count source Severity.Error !error_count;
  if has_errors then exit 1;

  result

let elab_tm ~(source : Source_file.t) : (Core.tm * Core.ty) option Surface.Elab.elab =
  fun ~report ->
    let lexbuf = Sedlexing.Utf8.from_string source.contents in
    let lex_error msg = report (Diagnostic.error (Sedlexing.lexing_positions lexbuf) msg) in
    Sedlexing.set_filename lexbuf source.name;

    match
      MenhirLib.Convert.Simplified.traditional2revised Parser.main
        (Sedlexing.with_tokenizer Lexer.token lexbuf)
    with
    | tm -> Surface.Elab.infer_tm tm ~report
    | exception Lexer.Error message -> lex_error message; None
    | exception Parser.Error -> lex_error "syntax error"; None


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = emit_diagnostics source (elab_tm ~source) |> Option.get in
  Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
    (Core.pp_tm [] tm)
    (Core.pp_ty ty)

let norm_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = emit_diagnostics source (elab_tm ~source) |> Option.get in
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
