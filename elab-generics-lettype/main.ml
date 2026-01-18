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

let emit (source : Source_file.t) (severity : string) (start, stop : Surface.Span.t) (message : string) =
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
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline;
  Printf.eprintf "\n"

let parse_tm (source : Source_file.t) : (Surface.Tm.t, (Surface.Span.t * string) list) result =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  try
    Sedlexing.set_filename lexbuf source.name;
    Sedlexing.with_tokenizer Lexer.token lexbuf
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.main
    |> Result.ok
  with
  | Lexer.Error message -> Error [lexpos (), message]
  | Parser.Error -> Error [lexpos (), "syntax error"]


(** {1 Subcommands} *)

let exit_ok = 0
let exit_error = 1

let elab_cmd () : int =
  let open Result.Syntax in
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  match
    let* tm = parse_tm source in
    let+ (tm, ty) = Surface.Elab.infer_tm tm in
    Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
      Core.(Tm.pp Env.empty Env.empty tm)
      Core.(Ty.pp Env.empty ty)
  with
  | Ok () -> exit_ok
  | Error errors ->
      errors |> List.iter (fun (pos, message) -> emit source "error" pos message);
      exit_error

let eval_cmd () : int =
  let open Result.Syntax in
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  match
    let* tm = parse_tm source in
    let+ (tm, ty) = Surface.Elab.infer_tm tm in
    Format.printf "@[<2>@[%t@ :@]@ @[%t@]@]@."
      Core.(Tm.Value.pp (Core.Tm.eval Env.empty tm))
      Core.(Ty.pp Env.empty ty)
  with
  | Ok () -> exit_ok
  | Error errors ->
      errors |> List.iter (fun (pos, message) -> emit source "error" pos message);
      exit_error


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a term from standard input")
      Term.(const elab_cmd $ const ());
    Cmd.v (Cmd.info "eval" ~doc:"elaborate and evaluate a term from standard input")
      Term.(const eval_cmd $ const ());
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval' cmd)
