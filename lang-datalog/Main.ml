let print_error (pos : Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let parse_program filename in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;

  try
    Parser.program Lexer.token lexbuf
  with
  | Lexer.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "unexpected character";
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "syntax error";
      exit 1

let pp_print_binding ppf (var, term) =
  Format.fprintf ppf "@[%a@ :=@ %a@]"
    Datalog.pp_print_term var
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
    | results ->
        results |> List.iter (List.iteri
          (function
            | 0 -> Format.printf "@[  > %a@].@\n" pp_print_binding;
            | _ -> Format.printf "@[    %a@].@\n" pp_print_binding));
        Format.printf "  yes@\n";
        Format.printf "@\n";
  );