let bin = "variant-unification"
let package = "elab-variant-unification"

let generate_rules base =
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.stdout.tmp
         (run %%{bin:%s} elab))))

      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.stdout %s.stdout.tmp)))
    |}
    base base bin package base base

let () =
  Sys.readdir ".."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".txt")
  |> List.iter generate_rules
