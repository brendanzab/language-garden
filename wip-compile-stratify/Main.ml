(* TODO: Proper test suite *)
(* TODO: Pretty printing *)

let () =
  let module R = CoreRules in

  Printexc.record_backtrace true;


  (* Basic tests *)

  let _, tm = R.run_synth @@ R.Univ.univ L0 in
  let _ = Core.Validation.(synth Context.empty tm) in
  let _ = CoreToStratified.(translate Context.empty tm) in

  let _, tm = R.run_synth @@ R.Univ.fun_ (R.Univ.univ L0) @@ fun u -> u in
  let _ = Core.Validation.(synth Context.empty tm) in
  (* FIXME: bug: level 0 terms are too small to contain types *)
  (* let _ = CoreToStratified.(translate Context.empty tm) in *)

  let _, tm = R.run_synth @@ R.Structure.let_synth (R.Univ.univ L0) @@ fun u -> u in
  let _ = Core.Validation.(synth Context.empty tm) in
  (* FIXME: bug: level 2 terms are too large to be typable *)
  (* let _ = CoreToStratified.(translate Context.empty tm) in *)


  (* Identity function (synthesis) *)

  let id =
    R.Fun.intro_synth ~name:"A" ~ty:(R.is_ty @@ R.Univ.univ L0) @@ fun a ->
    R.Fun.intro_synth ~name:"x" ~ty:(R.is_ty a) @@ fun x -> x
  in

  let _, tm = R.run_synth id in
  let _ = Core.Validation.(synth Context.empty tm) in
  (* FIXME: bug: level 0 terms are too small to contain types *)
  (* let _ = CoreToStratified.(translate Context.empty tm) in *)

  let app =
    R.Structure.let_synth ~name:"id" id @@ fun id ->
    R.Fun.intro_synth ~name:"B" ~ty:(R.is_ty @@ R.Univ.univ L0) @@ fun b ->
      R.Fun.app id (R.check b)
  in

  let _, tm = R.run_synth app in
  let _ = Core.Validation.(synth Context.empty tm) in
  (* FIXME: bug: level 0 terms are too small to contain types *)
  (* let _ = CoreToStratified.(translate Context.empty tm) in *)


  (* Identity function (checking) *)

  let id_ty =
    R.Univ.fun_ (R.Univ.univ L0) @@ fun a ->
    R.Univ.fun_ a @@ fun _ -> a
  in
  let id =
    R.ann ~ty:(R.is_ty id_ty) @@
    R.Fun.intro_check ~name:"A" @@ fun _ ->
    R.Fun.intro_check ~name:"x" @@ fun x ->
      R.check x
  in

  let _, tm = R.run_synth id in
  let _ = Core.Validation.(synth Context.empty tm) in
  (* FIXME: bug: level 0 terms are too small to contain types *)
  (* let _ = CoreToStratified.(translate Context.empty tm) in *)


  let app_ty =
    R.Univ.fun_ (R.Univ.univ L0) @@ fun b ->
    R.Univ.fun_ b @@ fun _ -> b
  in
  let app =
    R.ann ~ty:(R.is_ty app_ty) @@
    R.Fun.intro_check ~name:"B" @@ fun b ->
      R.check (R.Fun.app id (R.check b))
  in

  let _, tm = R.run_synth app in
  let _ = Core.Validation.(synth Context.empty tm) in
  (* FIXME: bug: level 0 terms are too small to contain types *)
  (* let _ = CoreToStratified.(translate Context.empty tm) in *)

  ()
