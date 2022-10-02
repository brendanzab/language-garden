module Surface = Elab.Surface


(** CLI options *)

let usage out_channel program =
  Printf.fprintf out_channel "USAGE:\n";
  Printf.fprintf out_channel "    %s elab [--no-resugar]...\n" program;
  Printf.fprintf out_channel "    %s norm [--no-resugar]...\n" program;
  Printf.fprintf out_channel "    %s --help | -h\n" program

let exit_usage_error program =
  Printf.eprintf "error: unexpected CLI arguments\n";
  usage stderr program;
  exit 1

let exit_usage_help program =
  usage stdout program;
  exit 0


(** CLI option parsing *)

type flags = {
  resugar : bool;
}

type command =
  | Elab of flags
  | Norm of flags

let rec parse_flags program flags = function
  | "--no-resugar" :: args -> parse_flags program { resugar = true } args
  | _ :: _ -> exit_usage_error program
  | [] -> flags

let parse_command program = function
  | "elab" :: args -> Elab (parse_flags program { resugar = true } args)
  | "norm" :: args -> Norm (parse_flags program { resugar = true } args)
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

let parse_tm filename in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;
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

let infer context tm =
  try Surface.infer context tm with
  | Surface.Error message ->
      Printf.eprintf "error: %s\n" message;
      exit 1

let pp_def ~resugar context fmt (name, ty, tm) =
  let pp_tm = Surface.pp ~resugar context in
  let pp_name_ann fmt (name, ty) =
    Format.fprintf fmt "@[<2>@[%s :@]@ @[%a@]@]" name pp_tm ty
  in
  Format.fprintf fmt "@[<2>@[%a@ :=@]@ @[%a@]@]"
    pp_name_ann (name, ty)
    (Surface.pp ~resugar context) tm


(** Main entrypoint *)

let main () =
  Printexc.record_backtrace true;

  let args = parse_args (Array.to_list Sys.argv) in
  let context = Surface.initial_context in
  let (tm, ty) = infer context (parse_tm "<input>" stdin) in

  match args with
  | Elab { resugar } ->
      Format.printf "%a@\n" (pp_def ~resugar context)
        ("<input>", Surface.quote context ty, tm)
  | Norm { resugar } ->
      Format.printf "%a@\n" (pp_def ~resugar context)
        ("<input>", Surface.quote context ty, Surface.normalise context tm)

let () = main ()
