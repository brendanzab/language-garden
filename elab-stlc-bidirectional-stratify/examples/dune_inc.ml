let bin = "stlc-bidirectional-stratify"
let package = "elab-stlc-bidirectional-stratify"

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
  Sys.readdir ".." |> Array.iter @@ fun name ->
    Filename.chop_suffix_opt name ~suffix:".txt"
    |> Option.iter generate_rules
