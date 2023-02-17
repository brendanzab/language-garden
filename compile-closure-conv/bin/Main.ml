let run tm =

  Format.printf "@.@[<v 2>FunLang:@ %a@]@." (FunLang.pp_tm []) tm;

  let _ = FunLang.Validation.synth [] tm in
  let clos_tm = FunToClos.translate [] 0 0 tm in

  Format.printf "@.@[<v 2>ClosLang:@ %a@]@." (ClosLang.pp_tm []) clos_tm;
  Format.printf "@.";

  let _ = ClosLang.Validation.synth [] clos_tm in
  let _ = ClosLang.Semantics.eval [] clos_tm in ()


let () =
  Printexc.record_backtrace true;

  let tm = FunLang.Build.(
    let_ "a" int_ty (int_lit 1) @@ fun _ ->
    let_ "f" (fun_ty [int_ty] int_ty)
      (fun_lit "x" int_ty @@ fun x -> x)
      @@ fun f ->
    fun_apps f [int_lit 100]
  ) 0 in

  run tm;


  let tm = FunLang.Build.(
    let_ "a" int_ty (int_lit 1) @@ fun a ->
    let_ "f" (fun_ty [int_ty] int_ty)
      (fun_lit "x" int_ty @@ fun _ -> a)
      @@ fun f ->
    fun_apps f [int_lit 100]
  ) 0 in

  run tm;


  let tm = FunLang.Build.(
    let_ "a" int_ty (int_lit 1) @@ fun a ->
    let_ "f" (fun_ty [int_ty] int_ty)
      (fun_lit "x" int_ty @@ fun x ->
        let_ "y" int_ty (x + a) @@ fun y -> y)
      @@ fun f ->
    fun_apps f [int_lit 100]
  ) 0 in

  run tm;


  let tm = FunLang.Build.(
    let_ "x" int_ty (int_lit 1) @@ fun x ->
    let_ "y" int_ty (int_lit 2) @@ fun y ->
    let_ "z" int_ty (int_lit 3) @@ fun _ ->
    let_ "f" (fun_ty [int_ty; int_ty] int_ty)
      (fun_lit "w" int_ty @@ fun w ->
          x + y + w)
      @@ fun f ->
    fun_apps f [int_lit 100]
  ) 0 in

  run tm;


  let tm = FunLang.Build.(
    let_ "a" int_ty (int_lit 2) @@ fun a ->
    let_ "b" int_ty (int_lit 4) @@ fun _ ->
    let_ "c" int_ty (int_lit 7) @@ fun c ->
    let_ "d" int_ty (int_lit 8) @@ fun _ ->
    fun_lit "x" int_ty @@ fun x ->
      a * x + c
  ) 0 in

  run tm;

  let tm = FunLang.Build.(
    let_ "a" int_ty (int_lit 2) @@ fun a ->
    let_ "b" int_ty (int_lit 5) @@ fun b ->
    let_ "f" (fun_ty [int_ty; int_ty] int_ty)
      (fun_lit "x" int_ty @@ fun x ->
        fun_lit "y" int_ty @@ fun y ->
          a * x + b * y)
      @@ fun f ->
    fun_apps f [int_lit 7; int_lit 3]
  ) 0 in

  run tm;
