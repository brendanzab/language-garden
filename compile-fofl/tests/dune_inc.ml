let bin = "fofl"
let package = "compile-fofl"

let generate_rules base = begin

  [`Default; `Emit_tc] |> List.iter begin fun test ->

    let source =
      Printf.sprintf "%s.txt" base

    and target, args =
      match test with
      | `Default -> Printf.sprintf "%s.wat" base, "compile"
      | `Emit_tc -> Printf.sprintf "%s.tc.wat" base, "compile --emit-tail-calls"
    in

    Printf.printf "(rule";
    Printf.printf " (with-stdin-from ../%s" source;
    Printf.printf "  (with-stdout-to %s.tmp" target;
    Printf.printf "   (run %%{bin:%s} %s))))" bin args;

    Printf.printf "(rule";
    Printf.printf " (alias runtest)";
    Printf.printf " (package %s)" package;
    Printf.printf " (action (diff ../%s %s.tmp)))" target target;

  end;

end

let () =
  Sys.readdir ".." |> Array.iter @@ fun name ->
    Filename.chop_suffix_opt name ~suffix:".txt"
    |> Option.iter generate_rules
