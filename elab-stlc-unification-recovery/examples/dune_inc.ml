let bin = "stlc-unification-recovery"
let package = "elab-stlc-unification-recovery"

let generate_rules base =
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-accepted-exit-codes 1
         (with-stderr-to %s.stderr.tmp
          (run %%{bin:%s} elab)))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.stderr %s.stderr.tmp)))
    |}
    package base base

let () =
  Sys.readdir ".."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".txt")
  |> List.iter generate_rules
