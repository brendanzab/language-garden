(*

  A simple implementation of Matin-Löf Type Theory with records

  Inspiration

  - [Typechecking for higher-rank polymorphism](https://gist.github.com/mb64/f49ccb1bbf2349c8026d8ccf29bd158e)
  - [Bidirectional typechecking for higher-rank polymorphism](https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7)
  - [Very simple typechecker for MLTT](https://gist.github.com/mb64/4a49d710dcdd1875bebdbc59081acb85)

*)

module Lexer = Surface.Lexer
module Parser = Surface.Parser
module Elaborator = Surface.Elaborator

let print_error (pos : Lexing.position) message =
  Printf.fprintf stderr "%s:%d:%d: %s\n%!"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let print_result name ty expr =
  Format.printf "%a@." Pp.to_fmt
    (Pp.hvbox ~indent:2 (Pp.concat [
      Pp.hvbox ~indent:2 (Pp.concat [
        Pp.hvbox ~indent:2 (Pp.concat [
          Pp.text name;
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
    ]))

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

  let context = Elaborator.initial_context in
  match Elaborator.synth_term context term with
  | Ok (expr, ty) ->
      let ty = Elaborator.quote context ty in
      let expr =
        match mode with
        | `elab -> expr
        | `norm -> Elaborator.norm context expr
      in

      print_result "<input>" ty expr;
      exit 0
  | Error message ->
      print_endline ("error: " ^ message);
      exit 1

let () = main ()
