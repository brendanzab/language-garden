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
      SurfaceParser.main SurfaceLexer.token lexbuf
    with
    | SurfaceLexer.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        print_error pos "unexpected character";
        exit 1
    | SurfaceParser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        print_error pos "syntax error";
        exit 1
    in

  Surface.Elaboration.(
    let context = initial_context in
    match synth_term context term with
    | Ok (expr, ty) ->
        let expr' = eval context expr in
        let expr = quote context expr' in
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
              Pp.hvbox (Core.Pretty.term ty);
              Pp.space;
              Pp.text ":=";
            ]);
            Pp.space;
            Pp.hvbox (Core.Pretty.term expr);
          ]));
        exit 0
    | Error message ->
        print_endline ("error: " ^ message);
        exit 1
  )

let () = main ()
