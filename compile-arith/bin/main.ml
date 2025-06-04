(** {0 Compiler CLI} *)

module Tree_lang = Arith.Tree_lang
module Stack_lang = Arith.Stack_lang
module Anf_lang = Arith.Anf_lang
module Tree_to_stack = Arith.Tree_to_stack
module Tree_to_anf = Arith.Tree_to_anf


module Source_file = struct

  type t = {
    name : string;
    contents : string;
    lines : (int * int) Dynarray.t
  }

  let from_channel (name : string) (ch : in_channel) : t =
    let buf = Buffer.create 16 in
    let lines = Dynarray.create () in

    let rec loop () =
      try
        let line = input_line ch in
        if not (Dynarray.is_empty lines) then Buffer.add_char buf '\n';
        Dynarray.add_last lines (Buffer.length buf, String.length line);
        Buffer.add_string buf line;
        loop ()
      with
      | End_of_file ->
          let contents = Buffer.contents buf in
          { name; contents; lines }
    in

    loop ()

  let get_line (source : t) (line : int) : string =
    let pos, len = Dynarray.get source.lines (line - 1) in
    String.sub source.contents pos len

end

let print_error (source : Source_file.t) (start, stop : Lexing.position * Lexing.position) (message : string) =
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "error: %s\n" message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline

let parse_expr (source : Source_file.t) : Tree_lang.expr =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf source.name;

  try
    MenhirLib.Convert.Simplified.traditional2revised Tree_lang.Parser.main
      (Sedlexing.with_tokenizer Tree_lang.Lexer.token lexbuf)
  with
  | Tree_lang.Lexer.Error -> print_error source (lexpos ()) "unexpected character"; exit 1
  | Tree_lang.Parser.Error -> print_error source (lexpos ()) "syntax error"; exit 1


(** {1 Subcommands} *)

type compile_target = [`Stack | `Anf]

type exec_target = [`Tree | `Stack | `Anf]

let compile : compile_target -> unit =
  function
  | `Stack ->
      let e = parse_expr (Source_file.from_channel "<stdin>" stdin) in
      let c = Tree_to_stack.translate e in
      Format.printf "@[<v>%a@]" Stack_lang.pp_code c
  | `Anf ->
      let e = parse_expr (Source_file.from_channel "<stdin>" stdin) in
      let e = Tree_to_anf.translate e in
      Format.printf "@[<v>%a@]" Anf_lang.pp_expr e

let exec : exec_target -> unit =
  function
  | `Tree ->
      let e = parse_expr (Source_file.from_channel "<stdin>" stdin) in
      Format.printf "%d" Tree_lang.Semantics.(eval e)
  | `Stack ->
      let e = parse_expr (Source_file.from_channel "<stdin>" stdin) in
      let c = Tree_to_stack.translate e in
      Format.printf "@[%a@]"
        Stack_lang.pp_code Stack_lang.Semantics.(normalise (c, []))
  | `Anf ->
      let e = parse_expr (Source_file.from_channel "<stdin>" stdin) in
      let e = Tree_to_anf.translate e in
      Format.printf "%d" Anf_lang.Semantics.(eval Env.empty e)


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let compile_target : compile_target Term.t =
    Arg.(required
      & opt (some & enum ["stack", `Stack; "anf", `Anf]) None
      & info ["target"] ~docv:"TARGET"
          ~doc:"The target language to compile to. The value of $(docv) must \
                be one of $(b,stack) or $(b,anf).")

  and exec_target : exec_target Term.t =
    Arg.(value
      & opt (enum ["tree", `Tree; "stack", `Stack; "anf", `Anf]) `Stack
      & info ["target"] ~docv:"TARGET"
          ~doc:"The language to use when interpreting expressions. The value \
                of $(docv) must be one of $(b,tree), $(b,stack) or $(b,anf).")
  in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "compile" ~doc:"compile an arithmetic expression")
      Term.(const compile $ compile_target);
    Cmd.v (Cmd.info "exec" ~doc:"run an arithmetic expression")
      Term.(const exec $ exec_target);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
