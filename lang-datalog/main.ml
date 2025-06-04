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

let print_error (source : Source_file.t) (start, stop : Lexing.position * Lexing.position) (message : string) =
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
      | `Unexpected_char -> print_error source (lexpos ()) "unexpected character"; exit 1
      | `Unclosed_string_literal -> print_error source (lexpos ()) "unclosed string literal"; exit 1
      | `Invalid_escape_code s -> print_error source (lexpos ()) (Format.sprintf "invalid escape code `\\%s`" s); exit 1
      end
  | Parser.Error -> print_error source (lexpos ()) "syntax error"; exit 1

let pp_print_binding ppf (var, term) =
  Format.fprintf ppf "@[%a@ :=@ %a@]"
    Format.pp_print_string var
    Datalog.pp_print_term term

let () =
  Printexc.record_backtrace true;

  (* TODO: Interactive REPL *)

  let source = Source_file.from_channel "<stdin>" stdin in
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
  kb |> List.iter (Format.printf "@[%a.@]@\n" Datalog.pp_print_atom);
  Format.printf "@\n";

  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  Format.printf "Query Results@\n";
  Format.printf "────────────────────────────────────────────────────────────────────────────────@\n";
  program.queries |> List.iter (fun query ->
    Format.printf "@[%a@]@\n" Datalog.pp_print_query query;
    match Datalog.run_query query kb with
    | [] ->
        Format.printf "  no@\n";
        Format.printf "@\n";
    | solutions ->
        solutions |> List.iter (List.iteri
          (function
            | 0 -> Format.printf "@[  > %a@].@\n" pp_print_binding;
            | _ -> Format.printf "@[    %a@].@\n" pp_print_binding));
        Format.printf "  yes@\n";
        Format.printf "@\n";
  );
