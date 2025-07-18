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

let emit (source : Source_file.t) (error : Surface.Error.t) =
  let start, stop = error.span in
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "error: %s\n" error.message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline;
  error.details |> List.iter begin fun message ->
    String.split_on_char '\n' message |> List.iteri begin fun i line ->
      match i with
      | 0 -> Printf.eprintf "%s = %s\n" gutter_pad line
      | _ -> Printf.eprintf "%s   %s\n" gutter_pad line
    end;
  end;
  Printf.eprintf "\n"

let parse_tm (source : Source_file.t) : Surface.tm =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  Sedlexing.set_filename lexbuf source.name;
  let report_fatal message =
    let span = Sedlexing.lexing_positions lexbuf in
    emit source (Surface.Error.make span message);
    exit 1
  in

  try
    MenhirLib.Convert.Simplified.traditional2revised Parser.main
      (Sedlexing.with_tokenizer Lexer.token lexbuf)
  with
  | Lexer.Error message -> report_fatal message
  | Parser.Error -> report_fatal "syntax error"

let elab_tm (source : Source_file.t) (tm : Surface.tm) : Core.tm * Core.ty =
  match Surface.Elab.infer_tm tm with
  | Ok (tm, ty) -> tm, ty
  | Error errors ->
      errors |> List.iter (emit source);
      exit 1


(** {1 Subcommands} *)

let elab_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = parse_tm source |> elab_tm source in
  Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
    (Core.pp_tm [] tm)
    (Core.pp_ty ty)

let norm_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let tm, ty = parse_tm source |> elab_tm source in
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
