(** {0 Compiler CLI} *)

module TreeLang = Arith.TreeLang
module StackLang = Arith.StackLang
module AnfLang = Arith.AnfLang
module TreeToStack = Arith.TreeToStack
module TreeToAnf = Arith.TreeToAnf


(** {1 Helper functions} *)

let print_error (start, _ : Lexing.position * Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    message

let parse_expr filename in_channel =
  let lexbuf = Sedlexing.Utf8.from_channel in_channel in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer TreeLang.Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised TreeLang.Parser.main
  with
  | TreeLang.Lexer.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "unexpected character";
      exit 1
  | TreeLang.Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1


(** {1 Subcommands} *)

type compile_target = [`Stack | `Anf]

type exec_target = [`Tree | `Stack | `Anf]

let compile : compile_target -> unit =
  function
  | `Stack ->
      let e = parse_expr "<input>" stdin in
      let c = TreeToStack.translate e in
      Format.printf "@[<v>%a@]" StackLang.pp_code c
  | `Anf ->
      let e = parse_expr "<input>" stdin in
      let e = TreeToAnf.translate e in
      Format.printf "@[<v>%a@]" AnfLang.pp_expr e

let exec : exec_target -> unit =
  function
  | `Tree ->
      let e = parse_expr "<input>" stdin in
      Format.printf "%d" TreeLang.Semantics.(eval e)
  | `Stack ->
      let e = parse_expr "<input>" stdin in
      let c = TreeToStack.translate e in
      Format.printf "@[%a@]"
        StackLang.pp_code StackLang.Semantics.(normalise (c, []))
  | `Anf ->
      let e = parse_expr "<input>" stdin in
      let e = TreeToAnf.translate e in
      Format.printf "%d" AnfLang.Semantics.(eval Env.empty e)


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

  Cmd.group (Cmd.info "arith") [
    Cmd.v (Cmd.info "compile" ~doc:"compile an arithmetic expression")
      Term.(const compile $ compile_target);
    Cmd.v (Cmd.info "exec" ~doc:"run an arithmetic expression")
      Term.(const exec $ exec_target);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
