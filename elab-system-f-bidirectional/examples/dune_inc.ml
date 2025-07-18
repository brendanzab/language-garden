let bin = "system-f-bidirectional"
let package = "elab-system-f-bidirectional"

let generate_rules base =
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.stdout.tmp
         (run %%{bin:%s} elab))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.stdout %s.stdout.tmp)))
    |}
    package base base

let () =
  Sys.readdir ".."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".txt")
  |> List.iter generate_rules
