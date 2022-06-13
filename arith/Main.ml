(*
  Compilation from tree-based arithmetic expressions to stack-based instructions

  - https://blog.andrepopovitch.com/zinc/
  - https://www.marigold.dev/post/efficiently-implementing-the-lambda-calculus-with-zinc
*)

let print_error (pos : Lexing.position) message =
  Printf.fprintf stderr "%s:%d:%d: %s\n%!"
      pos.pos_fname
      pos.pos_lnum
      pos.pos_cnum
      message

let main () =
  let term =
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

  let program =
    Compiler.TreeToStack.translate term in

  program |> List.iter (function
    | StackLang.Num n -> Printf.fprintf stdout "num %d\n" n
    | StackLang.Add -> Printf.fprintf stdout "add\n"
    | StackLang.Sub -> Printf.fprintf stdout "sub\n")

let () = main ()
