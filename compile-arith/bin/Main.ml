(** CLI options *)

let usage out_channel program =
  Printf.fprintf out_channel "USAGE:\n";
  Printf.fprintf out_channel "    %s compile\n" program;
  Printf.fprintf out_channel "    %s exec [--tree | --stack]\n" program;
  Printf.fprintf out_channel "    %s --help | -h\n" program

let exit_usage_error program =
  Printf.eprintf "error: unexpected CLI arguments\n";
  usage stderr program;
  exit 1

let exit_usage_help program =
  usage stdout program;
  exit 0


(** CLI option parsing *)

type lang =
  | Tree
  | Stack

type command =
  | Compile
  | Exec of lang

let parse_exec_flags program = function
  | ["--tree"] -> Tree
  | ["--stack"] -> Stack
  | [] -> Stack
  | _ -> exit_usage_error program

let parse_command program = function
  | ["compile"] -> Compile
  | "exec" :: args -> Exec (parse_exec_flags program args)
  | ["--help" | "-h"] -> exit_usage_help program
  | _ -> exit_usage_error program

let parse_args = function
  | program :: args -> parse_command (Filename.basename program) args
  | [] -> exit_usage_error "main"


(** Helper functions *)

let print_error (pos : Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let parse_expr filename in_channel =
  let open Arith in

  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;

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


(** Main entrypoint *)

let main () =
  let open Arith in

  Printexc.record_backtrace true;

  let args = parse_args (Array.to_list Sys.argv) in
  let expr = parse_expr "<input>" stdin in
  let code = TreeToStack.translate expr in

  match args with
    | Compile ->
        let code = TreeToStack.translate expr in
        Format.printf "@[%a@]" StackLang.pp_code code;
        exit 0
    | Exec Tree ->
        Format.printf "%d" (TreeLang.Semantics.eval expr);
        exit 0
    | Exec Stack ->
        Format.printf "%s"
          (StackLang.Semantics.eval code
            |> List.map string_of_int
            |> String.concat "_");
        exit 0


let () = main ()
