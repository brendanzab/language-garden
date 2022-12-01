let () =
  let open CoreRules in

  Printexc.record_backtrace true;

  (* TODO: Pretty printing *)

  let _, tm = run_synth @@ Univ.univ L0 in
  let _ = Core.Validation.(synth Context.empty tm) in

  let _, tm = run_synth @@ Univ.fun_ (Univ.univ L0) @@ fun u -> u in
  let _ = Core.Validation.(synth Context.empty tm) in

  let _, tm = run_synth @@ Structure.let_synth (Univ.univ L0) @@ fun u -> u in
  let _ = Core.Validation.(synth Context.empty tm) in


  (* Identity function (synthesis) *)

  let id =
    Fun.intro_synth ~name:"A" (is_ty @@ Univ.univ L0) @@ fun a ->
    Fun.intro_synth ~name:"x" (is_ty a) @@ fun x -> x
  in

  let _, tm = run_synth id in
  let _ = Core.Validation.(synth Context.empty tm) in

  let app =
    Structure.let_synth ~name:"id" id @@ fun id ->
    Fun.intro_synth ~name:"B" (is_ty @@ Univ.univ L0) @@ fun b ->
      Fun.app id (check b)
  in

  let _, tm = run_synth app in
  let _ = Core.Validation.(synth Context.empty tm) in


  (* Identity function (checking) *)

  let id_ty =
    Univ.fun_ (Univ.univ L0) @@ fun a ->
    Univ.fun_ a @@ fun _ -> a
  in
  let id =
    ann ~ty:(is_ty id_ty) @@
      Fun.intro_check ~name:"A" @@ fun _ ->
      Fun.intro_check ~name:"x" @@ fun x ->
        check x
  in

  let _, tm = run_synth id in
  let _ = Core.Validation.(synth Context.empty tm) in


  let app_ty =
    Univ.fun_ (Univ.univ L0) @@ fun b ->
    Univ.fun_ b @@ fun _ -> b
  in
  let app =
    ann ~ty:(is_ty app_ty) @@
      Fun.intro_check ~name:"B" @@ fun b ->
        check (Fun.app id (check b))
  in

  let _, tm = run_synth app in
  let _ = Core.Validation.(synth Context.empty tm) in

  ()
