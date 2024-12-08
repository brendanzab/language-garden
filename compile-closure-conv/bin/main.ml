module Lang = ClosureConv.Lang
module Translation = ClosureConv.Translation


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
    |> Sedlexing.with_tokenizer Lang.Fun.Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Lang.Fun.Parser.main
  with
  | Lang.Fun.Lexer.Error `UnexpectedChar ->
      print_error (Sedlexing.lexing_positions lexbuf) "unexpected character";
      exit 1
  | Lang.Fun.Lexer.Error `UnclosedBlockComment ->
      print_error (Sedlexing.lexing_positions lexbuf) "unclosed block comment";
      exit 1
  | Lang.Fun.Parser.Error ->
      print_error (Sedlexing.lexing_positions lexbuf) "syntax error";
      exit 1


(** {1 Subcommands} *)

type compile_target = [
  | `Clos
  | `FunA
  | `ClosA
  | `LiftedA
]

let compile : compile_target -> unit =
  function
  | `Clos ->
        let fun_tm = parse_expr "<input>" stdin in

        let _ = Lang.Fun.Validation.synth [] fun_tm in
        let clos_tm = Translation.FunToClos.translate [] 0 0 fun_tm in

        Format.printf "@[<v>%a@]@." (Lang.Clos.pp_tm []) clos_tm;

        let _ = Lang.Clos.Validation.synth [] clos_tm in
        let _ = Lang.Clos.Semantics.eval [] clos_tm in ()

  | `FunA ->
      let fun_tm = parse_expr "<input>" stdin in

      let _ = Lang.Fun.Validation.synth [] fun_tm in
      let funa_tm = Translation.FunToFunA.translate [] fun_tm in

      Format.printf "@[<v>%a@]@." Lang.FunA.pp_tm funa_tm;

      let _ = Lang.FunA.Validation.synth Lang.FunA.VarMap.empty funa_tm in
      let _ = Lang.FunA.Semantics.eval Lang.FunA.VarMap.empty funa_tm in ()

  | `ClosA ->
      let fun_tm = parse_expr "<input>" stdin in

      let _ = Lang.Fun.Validation.synth [] fun_tm in
      let funa_tm = Translation.FunToFunA.translate [] fun_tm in
      let _ = Lang.FunA.Validation.synth Lang.FunA.VarMap.empty funa_tm in
      let closa_tm = Translation.FunAToClosA.translate Lang.FunA.VarMap.empty funa_tm in

      Format.printf "@[<v>%a@]@." (Lang.ClosA.pp_tm) closa_tm;

      let _ = Lang.ClosA.Validation.synth Lang.ClosA.VarMap.empty closa_tm in
      let _ = Lang.ClosA.Semantics.eval Lang.ClosA.VarMap.empty closa_tm in ()

  | `LiftedA ->
      let fun_tm = parse_expr "<input>" stdin in

      let _ = Lang.Fun.Validation.synth [] fun_tm in
      let funa_tm = Translation.FunToFunA.translate [] fun_tm in
      let _ = Lang.FunA.Validation.synth Lang.FunA.VarMap.empty funa_tm in
      let lifteda_tm = Translation.FunAToLiftedA.translate funa_tm in

      Format.printf "@[<v>%a@]@." (Lang.LiftedA.pp_lifted_tm) lifteda_tm;

      let _ = Lang.LiftedA.Validation.synth_lifted lifteda_tm in
      let _ = Lang.LiftedA.Semantics.eval_lifted lifteda_tm in ()


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let targets = [
    "clos", `Clos;
    "fun-a", `FunA;
    "clos-a", `ClosA;
    "lifted-a", `LiftedA;
  ] in

  let compile_target : compile_target Term.t =
    Arg.(required
      & opt (some & enum targets) None
      & info ["target"] ~docv:"TARGET"
          ~doc:"The target language to compile to. The value of $(docv) must \
                be one of $(b,clos), $(b,fun-a), $(b,clos-a) or $(b,lifted-a).")
  in

  Cmd.group (Cmd.info "compile-closure-conv") [
    Cmd.v (Cmd.info "compile" ~doc:"compile an arithmetic expression")
      Term.(const compile $ compile_target);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
