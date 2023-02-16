(** {1 Helper functions} *)

let print_error (pos : Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let parse_expr filename in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;

  try
    FunParser.main FunLexer.token lexbuf
  with
  | FunLexer.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "unexpected character";
      exit 1
  | FunLang.UnboundName n ->
      (* FIXME: Incorrect position *)
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos (Format.sprintf "unbound name `%s`" n);
      exit 1
  | FunParser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "syntax error";
      exit 1


let () =
  Printexc.record_backtrace true;

  let tm = parse_expr "<input>" stdin in

  let _ = FunLang.Validation.synth [] tm in
  let clos_tm = FunToClos.translate [] 0 0 tm in

  Format.printf "@[<v>%a@]@." (ClosLang.pp_tm []) clos_tm;

  let _ = ClosLang.Validation.synth [] clos_tm in
  let _ = ClosLang.Semantics.eval [] clos_tm in ()
