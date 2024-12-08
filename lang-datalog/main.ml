let print_error (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    message

let parse_program filename in_channel =
  let lexbuf = Sedlexing.Utf8.from_channel in_channel in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
  with
  | Lexer.Error error ->
      let msg =
        match error with
        | `Unexpected_char -> "unexpected character"
        | `Unclosed_string_literal -> "unclosed string literal"
        | `Invalid_escape_code s -> Format.sprintf "invalid escape code `\\%s`" s
      in
      print_error (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1

let pp_print_binding ppf (var, term) =
  Format.fprintf ppf "@[%a@ :=@ %a@]"
    Format.pp_print_string var
    Datalog.pp_print_term term

let () =
  Printexc.record_backtrace true;

  (* TODO: Interactive REPL *)

  let program = parse_program "<input>" stdin in

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
