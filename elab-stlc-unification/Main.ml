(** {0 Elaborator CLI} *)

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
    Parser.main Lexer.token lexbuf
  with
  | Lexer.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "unexpected character";
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "syntax error";
      exit 1


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let tm = parse_expr "<input>" stdin in

  let tm, ty =
    try Surface.infer [] tm with
    | Surface.Error ((start, _), msg) ->
        print_error start msg;
        exit 1
  in

  match Surface.unsolved_metas () with
  | [] ->
      Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
        (Core.pp_tm []) (Core.zonk_tm tm)
        Core.pp_ty (Core.zonk_ty ty)
  | unsolved_metas ->
      unsolved_metas |> List.iter (function
        | `FunParam (start, _) -> print_error start "ambiguous function parameter"
        | `FunApp (start, _) -> print_error start "ambiguous function application");
      exit 1
