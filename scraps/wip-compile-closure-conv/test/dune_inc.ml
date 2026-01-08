let bin = "closure-conv"
let package = "wip-compile-closure-conv"

let generate_rules base =
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.clos.stdout.tmp
         (run %%{bin:%s} compile --target=clos))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.clos.stdout %s.clos.stdout.tmp)))
    |}
    package base base;

  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.lifted-a.stdout.tmp
         (run %%{bin:%s} compile --target=lifted-a))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.lifted-a.stdout %s.lifted-a.stdout.tmp)))
    |}
    package base base

let () =
  Sys.readdir ".."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".txt")
  |> List.iter generate_rules
