module Lang = Closure_conv.Lang
module Translation = Closure_conv.Translation


(** {1 Helper functions} *)

let print_error (start, _ : Lexing.position * Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    message

let parse_expr filename in_channel =
  let lexbuf = Sedlexing.Utf8.from_channel in_channel in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer Lang.Fun.Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Lang.Fun.Parser.main
  with
  | Lang.Fun.Lexer.Error message -> print_error (lexpos ()) message; exit 1
  | Lang.Fun.Parser.Error -> print_error (lexpos ()) "syntax error"; exit 1


(** {1 Subcommands} *)

type compile_target = [
  | `Clos
  | `Fun_a
  | `Clos_a
  | `Lifted_a
]

let compile : compile_target -> unit =
  function
  | `Clos ->
        let fun_tm = parse_expr "<input>" stdin in

        let _ = Lang.Fun.Validation.synth [] fun_tm in
        let clos_tm = Translation.Fun_to_clos.translate [] 0 0 fun_tm in

        Format.printf "@[<v>%a@]@." (Lang.Clos.pp_tm []) clos_tm;

        let _ = Lang.Clos.Validation.synth [] clos_tm in
        let _ = Lang.Clos.Semantics.eval [] clos_tm in ()

  | `Fun_a ->
      let fun_tm = parse_expr "<input>" stdin in

      let _ = Lang.Fun.Validation.synth [] fun_tm in
      let funa_tm = Translation.Fun_to_fun_a.translate [] fun_tm in

      Format.printf "@[<v>%a@]@." Lang.Fun_a.pp_tm funa_tm;

      let _ = Lang.Fun_a.Validation.synth Lang.Fun_a.Var_map.empty funa_tm in
      let _ = Lang.Fun_a.Semantics.eval Lang.Fun_a.Var_map.empty funa_tm in ()

  | `Clos_a ->
      let fun_tm = parse_expr "<input>" stdin in

      let _ = Lang.Fun.Validation.synth [] fun_tm in
      let funa_tm = Translation.Fun_to_fun_a.translate [] fun_tm in
      let _ = Lang.Fun_a.Validation.synth Lang.Fun_a.Var_map.empty funa_tm in
      let closa_tm = Translation.Fun_a_to_clos_a.translate Lang.Fun_a.Var_map.empty funa_tm in

      Format.printf "@[<v>%a@]@." (Lang.Clos_a.pp_tm) closa_tm;

      let _ = Lang.Clos_a.Validation.synth Lang.Clos_a.Var_map.empty closa_tm in
      let _ = Lang.Clos_a.Semantics.eval Lang.Clos_a.Var_map.empty closa_tm in ()

  | `Lifted_a ->
      let fun_tm = parse_expr "<input>" stdin in

      let _ = Lang.Fun.Validation.synth [] fun_tm in
      let funa_tm = Translation.Fun_to_fun_a.translate [] fun_tm in
      let _ = Lang.Fun_a.Validation.synth Lang.Fun_a.Var_map.empty funa_tm in
      let lifteda_tm = Translation.Fun_a_to_lifted_a.translate funa_tm in

      Format.printf "@[<v>%a@]@." (Lang.Lifted_a.pp_lifted_tm) lifteda_tm;

      let _ = Lang.Lifted_a.Validation.synth_lifted lifteda_tm in
      let _ = Lang.Lifted_a.Semantics.eval_lifted lifteda_tm in ()


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let targets = [
    "clos", `Clos;
    "fun-a", `Fun_a;
    "clos-a", `Clos_a;
    "lifted-a", `Lifted_a;
  ] in

  let compile_target : compile_target Term.t =
    Arg.(required
      & opt (some & enum targets) None
      & info ["target"] ~docv:"TARGET"
          ~doc:"The target language to compile to. The value of $(docv) must \
                be one of $(b,clos), $(b,fun-a), $(b,clos-a) or $(b,lifted-a).")
  in

  Cmd.group (Cmd.info (Filename.basename Sys.argv.(0))) [
    Cmd.v (Cmd.info "compile" ~doc:"compile an arithmetic expression")
      Term.(const compile $ compile_target);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
