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

let parse_tm (source : Source_file.t) : Surface.tm =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf source.name;

  try
    MenhirLib.Convert.Simplified.traditional2revised Parser.main
      (Sedlexing.with_tokenizer Lexer.token lexbuf)
  with
  | Lexer.Error error ->
      begin match error with
      | `Unexpected_char -> emit source "error" (lexpos ()) "unexpected character"; exit 1
      | `Unclosed_block_comment -> emit source "error" (lexpos ()) "unclosed block comment"; exit 1
      end
  | Parser.Error -> emit source "error" (lexpos ()) "syntax error"; exit 1

let elab_tm (source : Source_file.t) (ctx : Surface.Elab.context) (tm : Surface.tm) =
  try Surface.Elab.infer ctx tm with
  | Surface.Elab.Error (pos, message) ->
      emit source "error" pos message;
      exit 1

let pp_def ~resugar context ppf (name, ty, tm) =
  let pp_tm = Surface.Elab.pp ~resugar context in
  let pp_name_ann ppf (name, ty) =
    Format.fprintf ppf "@[<2>@[%s :@]@ @[%a@]@]" name pp_tm ty
  in
  Format.fprintf ppf "@[<2>@[%a@ :=@]@ @[%a@]@]"
    pp_name_ann (name, ty)
    (Surface.Elab.pp ~resugar context) tm


(** {1 Subcommands} *)

let elab_cmd (no_resugar : bool) : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let ctx = Surface.Elab.initial_context in
  let (tm, ty) = elab_tm source ctx (parse_tm source) in
  Format.printf "%a@\n" (pp_def ~resugar:(not no_resugar) ctx)
    ("<stdin>", Surface.Elab.quote ctx ty, tm)

let norm_cmd (no_resugar : bool) : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let ctx = Surface.Elab.initial_context in
  let (tm, ty) = elab_tm source ctx (parse_tm source) in
  Format.printf "%a@\n" (pp_def ~resugar:(not no_resugar) ctx)
    ("<stdin>", Surface.Elab.quote ctx ty, Surface.Elab.normalise ctx tm)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let no_resugar : bool Term.t =
    Arg.(value & flag
      & info ["no-resugar"]
          ~doc:"disable resugaring in pretty printed terms")
  in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a term from standard input")
      Term.(const elab_cmd $ no_resugar);
    Cmd.v (Cmd.info "norm" ~doc:"elaborate and normalise a term from standard input")
      Term.(const norm_cmd $ no_resugar);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
