(** {0 Arithmetic expression compiler}

    Compilation from tree-based arithmetic expressions to stack-based instructions

    - https://blog.andrepopovitch.com/zinc/
    - https://www.marigold.dev/post/efficiently-implementing-the-lambda-calculus-with-zinc
*)

let print_error (pos : Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let main () = ArithCond.(
    let expr =
      let lexbuf = Lexing.from_channel stdin in
      Lexing.set_filename lexbuf "<input>";
      try
        TreeLang.Parser.main TreeLang.Lexer.token lexbuf
      with
      | TreeLang.Lexer.Error ->
          let pos = Lexing.lexeme_start_p lexbuf in
          print_error pos "unexpected character";
          exit 1
      | TreeLang.Parser.Error ->
          let pos = Lexing.lexeme_start_p lexbuf in
          print_error pos "syntax error";
          exit 1
    in

    let code =
      TreeToStack.translate expr
    in

    Format.printf "@[<v>";
    Format.printf "@[<2>@[code  =@]@ %a@]@;"
      StackLang.pp_code code;
    Format.printf "@[<2>@[tree  =@]@ %a@]@;"
      TreeLang.pp_expr (TreeLang.Semantics.normalise expr);
    Format.printf "@[<2>@[stack =@]@ %a@]@;"
      StackLang.pp_code (StackLang.Semantics.normalise code);
    Format.printf "@]"
  )

let () = main ()
