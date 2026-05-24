let bin = "fofl"
let package = "compile-fofl"

module type Test = sig
  val wat_file : string
  val wasm_file : string
  val compile_args : string
  val wat2wasm_args : string
  val wasm_validate_args : string
end

let generate_rules base = begin
  let txt_file = Printf.sprintf "%s.txt" base in

  [
    (module struct
      let wat_file = Printf.sprintf "%s.wat" base
      let wasm_file = Printf.sprintf "%s.wasm" base
      let compile_args = ""
      let wat2wasm_args = ""
      let wasm_validate_args = ""
    end);
    (module struct
      let wat_file = Printf.sprintf "%s.tc.wat" base
      let wasm_file = Printf.sprintf "%s.tc.wasm" base
      let compile_args = " --enable-tail-call"
      let wat2wasm_args = " --enable-tail-call"
      let wasm_validate_args = " --enable-tail-call"
    end);
  ]
  |> List.iter begin fun (module T : Test) ->

    Printf.printf "(rule\n";
    Printf.printf " (with-stdin-from ../%s\n" txt_file;
    Printf.printf "  (with-stdout-to %s.tmp\n" T.wat_file;
    Printf.printf "   (run %%{bin:%s} compile-wat%s))))\n" bin T.compile_args;
    Printf.printf "";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)" package;
    Printf.printf " (action (diff ../%s %s.tmp)))\n" T.wat_file T.wat_file;
    Printf.printf "";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)\n" package;
    Printf.printf " (target %s)\n" T.wasm_file;
    Printf.printf " (deps (:wat %s.tmp))\n" T.wat_file;
    Printf.printf " (action (run wat2wasm %%{wat} -o %%{target}%s)))\n" T.wat2wasm_args;
    Printf.printf "";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)\n" package;
    Printf.printf " (deps (:wasm %s))\n" T.wasm_file;
    Printf.printf " (action (run wasm-validate %%{wasm}%s)))\n" T.wasm_validate_args;
    Printf.printf "";

  end;

end

let () =
  Sys.readdir ".." |> Array.iter @@ fun name ->
    Filename.chop_suffix_opt name ~suffix:".txt"
    |> Option.iter generate_rules
