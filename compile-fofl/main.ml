(** Command line interface *)

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

let emit (source : Source_file.t) (error : Surface.Error.t) =
  let start, stop = error.span in
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "error: %s\n" error.message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline;
  error.details |> List.iter begin fun message ->
    String.split_on_char '\n' message |> List.iteri begin fun i line ->
      match i with
      | 0 -> Printf.eprintf "%s = %s\n" gutter_pad line
      | _ -> Printf.eprintf "%s   %s\n" gutter_pad line
    end;
  end;
  Printf.eprintf "\n"

let parse_module (source : Source_file.t) : Surface.Module.t =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf source.name;

  try
    Sedlexing.with_tokenizer Lexer.token lexbuf
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.main
  with
  | Lexer.Error message -> emit source (Surface.Error.make (lexpos ()) message); exit 1
  | Parser.Error -> emit source (Surface.Error.make (lexpos ()) "syntax error"); exit 1

let elab_module (source : Source_file.t) (mod_ : Surface.Module.t) =
  match Surface.Elab.check_module mod_ with
  | Ok mod_ -> mod_
  | Error error ->
      emit source error;
      exit 1


(** {1 Subcommands} *)

let compile_wat_cmd (enable_tail_call : bool) : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  parse_module source
  |> elab_module source
  |> Core_to_wasm.translate_module ~enable_tail_call
  |> Wasm.Output_wat.pp_module
  |> Format.printf "%t"

let compile_anf_cmd () : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  parse_module source
  |> elab_module source
  |> Core_to_anf.translate_module
  |> Anf.Module.pp
  |> Format.printf "%t"

let compile_llvm_cmd (output_format : [`Ll | `Dot]) : unit =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let module_ =
    parse_module source
    |> elab_module source
    |> Core_to_llvm.translate_module
  in
  match output_format with
  | `Ll -> Llvm.Output_ll.pp_module module_ |> Format.printf "%t"
  | `Dot -> Llvm.Output_dot.pp_module module_ Out_channel.stdout

(** {1 CLI options} *)

let cmd : unit Cmdliner.Cmd.t =
  let open Cmdliner in

  let enable_tail_call : bool Term.t =
    Arg.(value & flag
      & info ["enable-tail-call"]
          ~doc:"Output tail-calls in generated WebAssembly")
  and llvm_output_format : [`Ll | `Dot] Term.t =
    Arg.(value
      & opt (enum ["ll", `Ll; "dot", `Dot]) `Ll
      & info ["output-format"] ~docv:"FORMAT"
          ~doc:"The output format to emit. The value of $(docv) must be either \
                $(b,ll) for LLVM IR, or $(b,dot) for Graphviz DOT.")
  in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "compile-anf" ~doc:"Compile a module from standard input to A-normal form")
      Term.(const compile_anf_cmd $ const ());
    Cmd.v (Cmd.info "compile-llvm" ~doc:"Compile a module from standard input to LLVM IR")
      Term.(const compile_llvm_cmd $ llvm_output_format);
    Cmd.v (Cmd.info "compile-wat" ~doc:"Compile a module from standard input to WAT (WebAssembly Text Format)")
      Term.(const compile_wat_cmd $ enable_tail_call);
  ]

(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
