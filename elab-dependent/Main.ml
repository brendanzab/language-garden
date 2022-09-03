module Surface = Lib.Surface

(** Example terms for testing *)
module Examples = struct

  (* TODO: Implement a parser and convert to promote tests *)

  open Surface

  let stuff =
    Let ("id", None,
      FunLit (["A", Some Univ; "a", Some (Name "A")], Name "a"),

    Let ("Bool", None, FunType (["Out", Univ; "true", Name "Out"; "false", Name "Out"], Name "Out"),
    Let ("true", Some (Name "Bool"), FunLit (["Out", None; "true", None; "false", None], Name "true"),
    Let ("false", Some (Name "Bool"), FunLit (["Out", None; "true", None; "false", None], Name "false"),

    Let ("Option", Some (FunArrow (Univ, Univ)),
      FunLit (["A", None], FunType (["Out", Univ; "some", FunArrow (Name "A", Name "Out"); "none", Name "Out"], Name "Out")),
    Let ("none", Some (FunType (["A", Univ], FunApp (Name "Option", [Name "A"]))),
      FunLit (["A", None], FunLit (["Out", None; "some", None; "none", None], Name "none")),
    Let ("some", Some (FunType (["A", Univ], FunArrow (Name "A", FunApp (Name "Option", [Name "A"])))),
      FunLit (["A", None; "a", None], FunLit (["Out", None; "some", None; "none", None], FunApp (Name "some", [Name "a"]))),

      FunApp (Name "some", [FunApp (Name "Option", [Name "Bool"]);
        FunApp (Name "some", [Name "Bool"; Name "true"])]))))))))

end

let () =
  let context = Surface.initial_context in
  let tm, ty = Surface.infer context Examples.stuff in
  print_endline ("  inferred type    │ " ^ Surface.pretty_quoted context ty);
  print_endline ("  elaborated term  │ " ^ Surface.pretty context tm);
  print_endline ("  normalised term  │ " ^ Surface.pretty_quoted context (Surface.eval context tm));
  print_endline ""
