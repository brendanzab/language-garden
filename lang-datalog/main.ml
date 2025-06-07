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

let emit (source : Source_file.t) (severity : string) (start, stop : Lexing.position * Lexing.position) (message : string) =
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

let parse_program (source : Source_file.t) : Datalog.program =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf source.name;

  try
    MenhirLib.Convert.Simplified.traditional2revised Parser.program
      (Sedlexing.with_tokenizer Lexer.token lexbuf)
  with
  | Lexer.Error error ->
      begin match error with
      | `Unexpected_char -> emit source "error" (lexpos ()) "unexpected character"; exit 1
      | `Unclosed_string_literal -> emit source "error" (lexpos ()) "unclosed string literal"; exit 1
      | `Invalid_escape_code s -> emit source "error" (lexpos ()) (Format.sprintf "invalid escape code `\\%s`" s); exit 1
      end
  | Parser.Error -> emit source "error" (lexpos ()) "syntax error"; exit 1

let pp_print_binding var term ppf =
  Format.fprintf ppf "@[%s@ :=@ %t@]"
    var
    (Datalog.pp_print_term term)

let () =
  Printexc.record_backtrace true;

  (* TODO: Interactive REPL *)

  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let program = parse_program source in

  (* Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  Format.printf "Source Program@\n";
  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  program.rules |> List.iter (Format.printf "@[%a@]@\n" Datalog.pp_print_rule);
  program.queries |> List.iter (Format.printf "@[%a@]@\n" Datalog.pp_print_query);
  Format.printf "@\n"; *)

  let kb = Datalog.solve program.rules in

  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  Format.printf "Knowledge Base@\n";
  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  kb |> List.iter (fun a -> Format.printf "@[%t.@]@\n" (Datalog.pp_print_atom a));
  Format.printf "@\n";

  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  Format.printf "Query Results@\n";
  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  program.queries |> List.iter begin fun query ->
    Format.printf "@[%t@]@\n" (Datalog.pp_print_query query);
    match Datalog.run_query query kb with
    | [] ->
        Format.printf "  no@\n";
        Format.printf "@\n";
    | solutions ->
        solutions |> List.iter (List.iteri
          (fun i (var, term) ->
            match i with
            | 0 -> Format.printf "@[  > %t@].@\n" (pp_print_binding var term);
            | _ -> Format.printf "@[    %t@].@\n" (pp_print_binding var term)));
        Format.printf "  yes@\n";
        Format.printf "@\n";
  end;
