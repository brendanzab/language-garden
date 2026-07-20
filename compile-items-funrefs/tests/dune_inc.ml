let bin = "items-funrefs"
let package = "compile-items-funrefs"

let generate_rules base = begin
  let txt_file = Printf.sprintf "%s.txt" base in

  begin
    let anf_file = Printf.sprintf "%s.anf" base in

    Printf.printf "(rule\n";
    Printf.printf " (with-stdin-from ../%s\n" txt_file;
    Printf.printf "  (with-stdout-to %s.tmp\n" anf_file;
    Printf.printf "   (run %%{bin:%s} compile-anf))))\n" bin;
    Printf.printf "\n";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)" package;
    Printf.printf " (action (diff ../%s %s.tmp)))\n" anf_file anf_file;
    Printf.printf "\n";
  end;

  begin
    let ll_file = Printf.sprintf "%s.ll" base in  (* LLVM IR *)
    let bc_file = Printf.sprintf "%s.bc" base in  (* LLVM bitcode *)

    Printf.printf "(rule\n";
    Printf.printf " (with-stdin-from ../%s\n" txt_file;
    Printf.printf "  (with-stdout-to %s.tmp\n" ll_file;
    Printf.printf "   (run %%{bin:%s} compile-llvm))))\n" bin;
    Printf.printf "\n";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)" package;
    Printf.printf " (action (diff ../%s %s.tmp)))\n" ll_file ll_file;
    Printf.printf "\n";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)\n" package;
    Printf.printf " (target %s)\n" bc_file;
    Printf.printf " (deps (:ll %s.tmp))\n" ll_file;
    Printf.printf " (action (run llvm-as %%{ll} -o %%{target})))\n";
    Printf.printf "\n";
  end;

  let generate_wat_rules ~base ~compile_args ~wat2wasm_args ~wasm_validate_args =
    let wat_file = Printf.sprintf "%s.wat" base in
    let wasm_file = Printf.sprintf "%s.wasm" base in

    Printf.printf "(rule\n";
    Printf.printf " (with-stdin-from ../%s\n" txt_file;
    Printf.printf "  (with-stdout-to %s.tmp\n" wat_file;
    Printf.printf "   (run %%{bin:%s} compile-wat%s))))\n" bin compile_args;
    Printf.printf "\n";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)" package;
    Printf.printf " (action (diff ../%s %s.tmp)))\n" wat_file wat_file;
    Printf.printf "\n";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)\n" package;
    Printf.printf " (target %s)\n" wasm_file;
    Printf.printf " (deps (:wat %s.tmp))\n" wat_file;
    Printf.printf " (action (run wat2wasm %%{wat} -o %%{target}%s)))\n" wat2wasm_args;
    Printf.printf "\n";
    Printf.printf "(rule\n";
    Printf.printf " (alias runtest)\n";
    Printf.printf " (package %s)\n" package;
    Printf.printf " (deps (:wasm %s))\n" wasm_file;
    Printf.printf " (action (run wasm-validate %%{wasm}%s)))\n" wasm_validate_args;
    Printf.printf "\n";
  in

  generate_wat_rules
    ~base:base
    ~compile_args:""
    ~wat2wasm_args:""
    ~wasm_validate_args:"";

  generate_wat_rules
    ~base:(Printf.sprintf "%s.tail-call" base)
    ~compile_args:" --enable-tail-call"
    ~wat2wasm_args:" --enable-tail-call"
    ~wasm_validate_args:" --enable-tail-call";

end

let () =
  Sys.readdir ".." |> Array.iter @@ fun name ->
    Filename.chop_suffix_opt name ~suffix:".txt"
    |> Option.iter generate_rules
