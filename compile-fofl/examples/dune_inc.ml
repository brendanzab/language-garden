let bin = "fofl"
let package = "compile-fofl"

let generate_rules base = begin
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.wat.tmp
         (run %%{bin:%s} compile))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.wat %s.wat.tmp)))
    |}
    package base base;

  (* with tail-calls *)
  Printf.printf
    {|
      (rule
       (with-stdin-from ../%s.txt
        (with-stdout-to %s.tc.wat.tmp
         (run %%{bin:%s} compile --emit-tail-calls))))
    |}
    base base bin;
  Printf.printf
    {|
      (rule
       (alias runtest)
       (package %s)
       (action
        (diff ../%s.tc.wat %s.tc.wat.tmp)))
    |}
    package base base;
end

let () =
  Sys.readdir ".." |> Array.iter @@ fun name ->
    Filename.chop_suffix_opt name ~suffix:".txt"
    |> Option.iter generate_rules
