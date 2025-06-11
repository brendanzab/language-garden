(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

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

module Severity = struct

  type t =
    | Warning
    | Error

  let to_string ?(count : int option) (s : t) =
    match s, count with
    | Warning, None -> "warning"
    | Error, None -> "error"
    | Warning, Some n -> if n = 1 then "1 warning" else Format.sprintf "%i warnings" n
    | Error, Some n -> if n = 1 then "1 error" else Format.sprintf "%i errors" n

end

module Diagnostic = struct

  type t = {
    loc : loc;
    severity : Severity.t;
    message : string;
  }

  let error (loc : loc) (message : string) : t = { loc; severity = Error; message }
  let warning (loc : loc) (message : string) : t = { loc; severity = Warning; message }

end

module Effect = struct

  (** An action that accepts a callback for reporting diagnostics to the caller. *)
  type 'a t = report:(Diagnostic.t -> unit) -> 'a

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

  let emit_diagnostic_count (source : Source_file.t) (severity : Severity.t) (count : int) =
    Printf.eprintf "%s: %s generated %s\n"
      (Severity.to_string severity)
      source.name
      (Severity.to_string ~count severity)

  let run (type a) (source : Source_file.t) (prog : a t) : a =
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

end
