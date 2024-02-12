(** {0 Elaborator CLI} *)

(** {1 Helper functions} *)

let print_error (start, _ : Surface.loc) message =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    message

let parse_tm (filename : string) (input : in_channel) : Surface.tm =
  let lexbuf = Sedlexing.Utf8.from_channel input in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.main
  with
  | Lexer.Error error ->
      let msg =
        match error with
        | `UnexpectedChar -> "unexpected character"
        | `UnclosedBlockComment -> "unclosed block comment"
      in
      print_error (Sedlexing.lexing_positions lexbuf) msg;
      exit 1
  | Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1

let elab_tm (tm : Surface.tm) : Core.tm * Core.ty =
  let tm, ty =
    try Surface.elab_infer [] tm with
    | Surface.Error (pos, msg) ->
        print_error pos msg;
        exit 1
  in

  match Surface.unsolved_metas () with
  | [] ->
      Core.zonk_tm tm,
      Core.zonk_ty ty
  | unsolved_metas ->
      unsolved_metas |> List.iter (function
        | (pos, `FunParam) -> print_error pos "ambiguous function parameter type"
        | (pos, `FunBody) -> print_error pos "ambiguous function return type"
        | (pos, `IfBranches) -> print_error pos "ambiguous if expression branches"
        | (pos, `Placeholder) -> print_error pos "unsolved placeholder");
      exit 1


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;

  let tm = parse_tm "<input>" stdin in
  let tm, ty = elab_tm tm in

  Format.printf "@[<2>@[%a@ :@]@ @[%a@]@]@."
    (Core.pp_tm []) tm
    Core.pp_ty ty
