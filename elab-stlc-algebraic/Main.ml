(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (severity : string) (pos : Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      severity
      message

let parse_expr filename in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;

  try
    Parser.main Lexer.token lexbuf
  with
  | Lexer.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error "error" pos "unexpected character";
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error "error" pos "syntax error";
      exit 1


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let tm = parse_expr "<input>" stdin in

  let tm, ty =
    try Surface.elab_synth tm with
    | Surface.Error ((start, _), msg) ->
        print_error "error" start msg;
        exit 1
    | Surface.Bug ((start, _), msg) ->
        print_error "bug" start msg;
        exit 1
  in

  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    Core.pp_tm tm
    Core.pp_ty ty
