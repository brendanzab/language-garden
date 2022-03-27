(*
  Compilation from tree-based arithmetic expressions to stack-based instructions

  - https://blog.andrepopovitch.com/zinc/
  - https://www.marigold.dev/post/efficiently-implementing-the-lambda-calculus-with-zinc
*)

(** A translation pass between two languages *)
module type Translation = sig
  (** The source language *)
  type source

  (** The target language *)
  type target

  (** The translation pass between the {!source} and {!target} languages *)
  val translate : source -> target
end

(** Converts between the {!TreeArith} language and the {!StackArith} language *)
module TreeToStack : Translation

  with type source := TreeArith.term
  with type target := StackArith.program

= struct

  let rec translate : TreeArith.term -> StackArith.program =
    function
    | TreeArith.Num n -> [StackArith.Num n]
    | TreeArith.Add (n1, n2) -> translate n1 @ translate n2 @ [StackArith.Add]
    | TreeArith.Sub (n1, n2) -> translate n1 @ translate n2 @ [StackArith.Sub]

end

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

  let program = TreeToStack.translate term in
  program |> List.iter (function
    | StackArith.Num n -> Printf.fprintf stdout "num %d\n" n
    | StackArith.Add -> Printf.fprintf stdout "add\n"
    | StackArith.Sub -> Printf.fprintf stdout "sub\n")

let () = main ()
