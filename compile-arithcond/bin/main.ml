(** {0 Compiler CLI} *)

module Tree_lang = Arith_cond.Tree_lang
module Stack_lang = Arith_cond.Stack_lang
module Anf_lang = Arith_cond.Anf_lang
module Tree_to_stack = Arith_cond.Tree_to_stack
module Tree_to_anf = Arith_cond.Tree_to_anf


module Source_file = struct

  type t = {
    name : string;
    contents : string;
    lines : (int * int) Dynarray.t;
  }

  let create (name : string) (contents : string) : t =
    let lines = Dynarray.create () in
    let add_line stop =
      match Dynarray.find_last lines with
      | None -> Dynarray.add_last lines (0, stop)
      | Some (_, prev_stop) -> Dynarray.add_last lines (prev_stop + 1, stop)
    in
    contents |> String.iteri (fun pos ch -> if ch = '\n' then add_line pos);
    add_line (String.length contents);

    { name; contents; lines }

  let get_line (source : t) (line : int) : string =
    let start, stop = Dynarray.get source.lines (line - 1) in
    String.sub source.contents start (stop - start)

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
  | Tree_lang.Unbound_name (loc, n) -> print_error source loc (Format.sprintf "unbound name `%s`" n); exit 1
  | Tree_lang.Parser.Error -> print_error source (lexpos ()) "syntax error"; exit 1

let synth_expr ctx e =
  try
    Tree_lang.Validation.synth ctx e
  with
  | Tree_lang.Validation.Error message ->
      Printf.eprintf "error: %s" message;
      exit 1


(** {1 Subcommands} *)

type compile_target = [`Stack | `Anf]

type exec_target = [`Tree | `Stack | `Anf]

let compile : compile_target -> unit =
  function
  | `Stack ->
      let e = parse_expr (Source_file.create "<stdin>" (In_channel.input_all stdin)) in
      let _ = synth_expr [] e in
      let c = Tree_to_stack.translate e in
      Format.printf "@[<v>%a@]" Stack_lang.pp_code c
  | `Anf ->
      let e = parse_expr (Source_file.create "<stdin>" (In_channel.input_all stdin)) in
      let _ = synth_expr [] e in
      let e = Tree_to_anf.translate e in
      Format.printf "@[<v>%a@]" (Anf_lang.pp_expr []) e

let exec : exec_target -> unit =
  function
  | `Tree ->
      let e = parse_expr (Source_file.create "<stdin>" (In_channel.input_all stdin)) in
      let t = synth_expr [] e in
      Format.printf "@[<2>@[@[%a@]@ :@]@ %a@]"
        (Tree_lang.pp_expr []) Tree_lang.Semantics.(normalise [] e)
        Tree_lang.pp_ty t
  | `Stack ->
      let e = parse_expr (Source_file.create "<stdin>" (In_channel.input_all stdin)) in
      let _ = synth_expr [] e in
      let c = Tree_to_stack.translate e in
      Format.printf "@[%a@]"
        Stack_lang.pp_code Stack_lang.Semantics.(normalise (c, [], []))
  | `Anf ->
      let e = parse_expr (Source_file.create "<stdin>" (In_channel.input_all stdin)) in
      let t = synth_expr [] e in
      let e = Tree_to_anf.translate e in
      Format.printf "@[<2>@[@[%a@]@ :@]@ %a@]"
        (Anf_lang.pp_expr []) Anf_lang.Semantics.(normalise Env.empty e)
        Tree_lang.pp_ty t


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
