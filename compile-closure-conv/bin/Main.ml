module Lang = ClosureConv.Lang
module Translation = ClosureConv.Translation


(** {1 Helper functions} *)

let print_error (pos : Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      message

let parse_expr filename in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;

  try
    Lang.Fun.Parser.main Lang.Fun.Lexer.token lexbuf
  with
  | Lang.Fun.Lexer.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "unexpected character";
      exit 1
  | Lang.Fun.UnboundName n ->
      (* FIXME: Incorrect position *)
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos (Format.sprintf "unbound name `%s`" n);
      exit 1
  | Lang.Fun.Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      print_error pos "syntax error";
      exit 1


(** {1 Subcommands} *)

type compile_target = [
  | `Clos
  | `FunA
  | `ClosA
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


(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  let compile_target : compile_target Term.t =
    Arg.(required
      & opt (some & enum ["clos", `Clos; "fun-a", `FunA; "clos-a", `ClosA]) None
      & info ["target"] ~docv:"TARGET"
          ~doc:"The target language to compile to. The value of $(docv) must \
                be one of $(b,clos), $(b,fun-a) or $(b,clos-a).")
  in

  Cmd.group (Cmd.info "compile-closure-conv") [
    Cmd.v (Cmd.info "compile" ~doc:"compile an arithmetic expression")
      Term.(const compile $ compile_target);
  ]


(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
