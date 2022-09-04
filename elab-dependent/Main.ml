module Surface = Lib.Surface

let print_error (pos : Lexing.position) message =
  Printf.fprintf stderr "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let main () =
  let mode =
    match Array.to_list Sys.argv with
      | [ _; "elab" ] -> `elab
      | [ _; "norm" ] -> `norm
      | _ ->
          Printf.fprintf stderr "error: unexpected CLI arguments\n";
          Printf.fprintf stderr "Usage:\n";
          Printf.fprintf stderr "    mltt elab\n";
          Printf.fprintf stderr "    mltt norm\n";
          exit 1
    in

  let tm =
    let lexbuf = Lexing.from_channel stdin in
    Lexing.set_filename lexbuf "<input>";
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
    in

  let context = Surface.initial_context in
  let (tm, ty) =
    try Surface.infer context tm with
    | Surface.Error message ->
        print_endline ("error: " ^ message);
        exit 1
  in

  match mode with
  | `elab ->
      Printf.fprintf stdout "<input> : %s :=\n  %s\n"
        (Surface.pretty_quoted context ty)
        (Surface.pretty context tm)
  | `norm ->
      Printf.fprintf stdout "<input> : %s :=\n  %s\n"
        (Surface.pretty_quoted context ty)
        (Surface.pretty context (Surface.normalise context tm))

let () = main ()
