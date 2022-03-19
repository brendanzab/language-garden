(*

  An implementation of MLTT with records

  Inspiration

  - [Typechecking for higher-rank polymorphism](https://gist.github.com/mb64/f49ccb1bbf2349c8026d8ccf29bd158e)
  - [Bidirectional typechecking for higher-rank polymorphism](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7)
  - [Very simple typechecker for MLTT](https://gist.github.com/mb64/4a49d710dcdd1875bebdbc59081acb85)

  TODO:

  - [x] interpreter
  - [x] elaborator
    - [ ] unifier
  - [x] parser
  - [x] pretty printer
  - [ ] REPL
*)

let main () =
  let term =
    let source = Stdio.In_channel.(input_all stdin) in
    let lexbuf = Lexing.from_string source in
    try
      Surface_parser.main Surface_lexer.token lexbuf
    with
    | Surface_lexer.Error msg ->
        Printf.fprintf stderr "%s%!" msg;
        exit 1
    | Surface_parser.Error ->
        Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf);
        exit 1
    in

  Surface.Elaboration.(
    let context = initial_context in
    match synth_term context term with
    | Ok (expr, ty) ->
        let ty = quote context ty in
        Format.printf "%a@." Pp.to_fmt
          (Pp.hvbox ~indent:2 (Pp.concat [
            Pp.hvbox ~indent:2 (Pp.concat [
              Pp.hvbox ~indent:2 (Pp.concat [
                Pp.text "input";
                Pp.space;
                Pp.text ":";
              ]);
              Pp.space;
              Pp.hvbox (Core.pp_term ty);
              Pp.space;
              Pp.text ":=";
            ]);
            Pp.space;
            Pp.hvbox (Core.pp_term expr);
          ]));
        exit 0
    | Error message ->
        print_endline ("error: " ^ message);
        exit 1
  )

let () = main ()
