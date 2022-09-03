module Core = Lib.Core
module Surface = Lib.Surface

(** Example terms for testing *)
module Examples = struct

  open Surface

  (* TODO: Implement a parser and convert to promote tests *)

  (*
    let F := {
      A : Type;
      B : Type;
      f : A -> B;
    };

    let record-ty-patch-1 := (fun x := x) : F [ B := A ] -> F;
    let record-ty-patch-2 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F;
    let record-ty-patch-3 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A; f := fun x := x ] -> F;
    let record-ty-patch-3b := (fun A x := x) : fun (A : Type) -> F [ B := A; f := fun x := x; A := A; ] -> F;
    let record-ty-patch-4 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ B := A ];
    let record-ty-patch-5 := (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ A := A ];
    let record-ty-patch-6 := (fun C x := x) : fun (C : Type) -> F [ A := C; B := C ] -> F [ B := C ];

    let record-lit-missing-1 := (fun C := { f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ];
    let record-lit-missing-2 := (fun C := {}) : fun (C : Type) -> F [ A := C; B := C; f := fun x := x ];
    let record-lit-missing-3 := (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ];

    let record-lit-coerce-1 :=
      (fun B r := r) :
        fun (B : Type) (r : { A : Type [= B ]; a : B })
          -> { A : Type; a : A };
    let record-lit-coerce-2 :=
      (fun B b := { A := B; a := b } : { A : Type; a : B }) :
        fun (B : Type) (b : Type) -> { A : Type; a : A };

    let record-lit-coerce-missing-1 := (fun A B r := r) : fun (A : Type) (B : Type) -> { f : A -> B } -> F [ A := A; B := B ];
    let record-lit-coerce-missing-2 := (fun A B r := r) : fun (A : Type) (B : Type) -> { A : Type; f : A -> B } -> F [ B := B ];

    let intro-sing := (fun A x := x) : fun (A : Type) (x : A) -> A [= x ];
    let elim-sing := (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [= x ]) -> A;

    let sing-tm-1 :=
      (fun A P f prf := prf) :
        fun (A : Type)
            (P : (fun (x : A) -> A [= x ]) -> Type)
            (f : fun (x : A) -> A [= x ])
            (prf : P (fun x := x))
        -> P f;

    let let-ann-check :=
      (let id : fun (A : Type) -> A -> A :=
        fun A a := a; {}) : Type;

    let let-ann-infer :=
      let id : fun (A : Type) -> A -> A :=
        fun A a := a;
      id {} {};

    -- Example from page 4 of “1ML – Core and Modules United”
    let map-functor :=

      let Bool := fun (Out : Type) { true : Out; false : Out } -> Out;
      let true : Bool := fun Out cases := cases.true;
      let false : Bool := fun Out cases := cases.false;

      let Option : Type -> Type := fun A :=
        fun (Out : Type) { some : A -> Out; none : Out } -> Out;

      let none : fun (A : Type) -> Option A :=
        fun A := fun Out cases := cases.none;
      let some : fun (A : Type) -> A -> Option A :=
        fun A a := fun Out cases := cases.some a;

      let Eq := {
        T : Type;
        eq : T -> T -> Bool;
      };

      let Map := {
        Key : Type;
        Map : Type -> Type;
        empty : fun (A : Type) -> Map A;
        add : fun (A : Type) -> Key -> A -> Map A -> Map A;
        lookup : fun (A : Type) -> Key -> Map A -> Option A;
      };

      -- TODO: sealing operator
      let eq-map : fun (key : Eq) -> Map [ Key := key.T ] :=
        fun key := {
          Map := fun A := key.T -> Option A
          empty := fun A := fun x := none A;
          add := fun A k v map :=
            fun x := (key.eq x k) (Option A) {
              true := some A v;
              false := map x;
            };
          lookup := fun A k map := map k;
        };

      Type;

    -- TODO: requires total space conversion like in CoolTT

    let category := {
      Ob : Type;
      Hom : { s : Ob; t : Ob } -> Type;
      id : fun (x : Ob) -> Hom [ s := x; t := x ];
      seq : fun (f : Hom) (g : Hom [ s := f.t ]) -> Hom [ s := f.s; t := g.t ];
    };

    let types : category := {
      Ob := Type;
      Hom := fun params := params.s -> params.t;
      id := fun A a := a;
      seq := fun f g a := g (f a);
    };

    Type
  *)

  (* let F := { A : Type; B : Type; f : A -> B }; *)
  let fun_record_ty =
    RecType [
      "A", Univ;
      "B", Univ;
      "f", FunArrow (Name "A", Name "B");
    ]

  let record_ty_patch1 =
    Let ("F", None, fun_record_ty,
      (* (fun x := x) : F [ B := A ] -> F *)
      Ann (FunLit (["x"], Name "x"),
        FunArrow (Patch (Name "F", ["B", Name "A"]), Name "F")))

  let record_ty_patch2 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]), Name "F"))))

  let record_ty_patch3 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A; f := fun x := x ] -> F *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"; "f", FunLit (["x"], Name "x")]),
            Name "F"))))

  let record_ty_patch3b =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ B := A; f := fun x := x; A := A; ] -> F; *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["B", Name "A"; "f", FunLit (["x"], Name "x"); "A", Name "A"]),
            Name "F"))))

  let record_ty_patch4 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ B := A ] *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
            Patch (Name "F", ["B", Name "A"])))))

  let record_ty_patch5 =
    Let ("F", None, fun_record_ty,
      (* (fun A x := x) : fun (A : Type) -> F [ A := A; B := A ] -> F [ A := A ] *)
      Ann (FunLit (["A"; "x"], Name "x"),
        FunType (["A", Univ],
          FunArrow (Patch (Name "F", ["A", Name "A"; "B", Name "A"]),
            Patch (Name "F", ["A", Name "A"])))))

  let record_ty_patch6 =
    Let ("F", None, fun_record_ty,
      (* (fun C x := x) : fun (C : Type) -> F [ A := C; B := C ] -> F [ B := C ] *)
      Ann (FunLit (["C"; "x"], Name "x"),
        FunType (["C", Univ],
          FunArrow (Patch (Name "F", ["A", Name "C"; "B", Name "C"]),
            Patch (Name "F", ["B", Name "C"])))))

  let record_lit_missing1 =
    Let ("F", None, fun_record_ty,
      (* (fun C := { f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ] *)
      Ann (FunLit (["C"], RecLit ["f", FunLit (["x"], Name "x")]),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  let record_lit_missing2 =
    Let ("F", None, fun_record_ty,
      (* (fun C := {}) : fun (C : Type) -> F [ A := C; B := C; f := fun x := x ] *)
      Ann (FunLit (["C"], RecLit []),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"; "f", FunLit (["x"], Name "x")]))))

  let record_lit_missing3 =
    Let ("F", None, fun_record_ty,
      (* (fun C := { A := C; f := fun x := x }) : fun (C : Type) -> F [ A := C; B := C ] *)
      Ann (FunLit (["C"], RecLit ["B", Name "C"; "f", FunLit (["x"], Name "x")]),
        FunType (["C", Univ], Patch (Name "F", ["A", Name "C"; "B", Name "C"]))))

  (*
    (fun B r := r) :
      fun (B : Type) (r : { A : Type [= B ]; a : B })
        -> { A : Type; a : A }
  *)
  let record_lit_coerce1 =
    Ann (FunLit (["B"; "r"], Name "r"),
      FunType (["B", Univ; "r", RecType ["A", SingType (Univ, Name "B"); "a", Name "B"]],
        RecType ["A", Univ; "a", Name "A"]))

  (*
    (fun B b := { A := B; a := b } : { A : Type; a : B }) :
      fun (B : Type) (b : Type) -> { A : Type; a : A }
  *)
  let record_lit_coerce2 =
    Ann (FunLit (["B"; "b"],
        Ann (RecLit ["A", Name "B"; "a", Name "b"],
          RecType ["A", Univ; "a", Name "B"])),
      FunType (["B", Univ; "b", Name "B"], RecType ["A", Univ; "a", Name "A"]))

  (*
    (fun A B r := r) : fun (A : Type) (B : Type) -> { f : A -> B } -> F [ A := A; B := B ]
  *)
  let record_lit_coerce_missing1 =
    (* let F := { A : Type; B : Type; f : A -> B }; *)
    Let ("F", None, fun_record_ty,
      Ann (FunLit (["A"; "B"; "r"], Name "r"),
        FunType (["A", Univ; "B", Univ], FunArrow (RecType ["f", FunArrow (Name "A", Name "B")],
          Patch (Name "F", ["A", Name "A"; "B", Name "B"])))))

  (*
    (fun B r := r) : fun (B : Type) -> { A : Type; f : A -> B } -> F [ B := B ]
  *)
  let record_lit_coerce_missing2 =
    (* let F := { A : Type; B : Type; f : A -> B }; *)
    Let ("F", None, fun_record_ty,
      Ann (FunLit (["B"; "r"], Name "r"),
        FunType (["B", Univ], FunArrow (RecType ["A", Univ; "f", FunArrow (Name "A", Name "B")],
          Patch (Name "F", ["B", Name "B"])))))

  (*
     (fun A x := x) : fun (A : Type) (x : A) -> A [= x ]
  *)
  let intro_sing =
    Ann (FunLit (["A"; "x"], Name "x"),
      FunType (["A", Univ; "x", Name "A"], SingType (Name "A", Name "x")))

  (*
     (fun A x sing-x := sing-x) : fun (A : Type) (x : A) (sing-x : A [= x ]) -> A
  *)
  let elim_sing =
    Ann (FunLit (["A"; "x"; "sing-x"], Name "sing-x"),
      FunType (["A", Univ; "x", Name "A"; "sing-x", SingType (Name "A", Name "x")], Name "A"))

  (*
    (fun A P f pf := pf) :
      fun (A : Type)
          (P : (fun (x : A) -> A [= x ]) -> Type)
          (f : fun (x : A) -> A [= x ])
          (pf : P (fun x := x))
      -> P f
  *)
  let sing_tm1 =
    Ann (FunLit (["A"; "P"; "f"; "prf"], Name "prf"),
      FunType ([
        "A", Univ;
        "P", FunType (["_", FunType (["x", Name "A"], SingType (Name "A", Name "x"))], Univ);
        "f", FunType (["x", Name "A"], SingType (Name "A", Name "x"));
        "prf", App (Name "P", [FunLit (["x"], Name "x")]);
      ], App (Name "P", [Name "f"])))

  (*
    (let id : fun (A : Type) -> A -> A :=
      fun A a := a; {}) : Type
  *)
  let let_ann_check =
    Ann (Let ("id", Some (FunType (["A", Univ], FunArrow (Name "A", Name "A"))), FunLit (["A"; "a"], Name "a"),
      RecUnit), Univ)

  (*
    let id : fun (A : Type) -> A -> A :=
      fun A a := a;
    id {} {}
  *)
  let let_ann_infer =
    Let ("id", Some (FunType (["A", Univ], FunArrow (Name "A", Name "A"))), FunLit (["A"; "a"], Name "a"),
      App (Name "id", [RecUnit; RecUnit]))

  (*
    -- Example from page 4 of “1ML – Core and Modules United”

    let Bool := fun (Out : Type) (cases : { true : Out; false : Out }) -> Out;
    let true : Bool := fun Out cases := cases.true;
    let false : Bool := fun Out cases := cases.false;

    let Option : Type -> Type := fun A :=
      fun (Out : Type) (cases : { some : A -> Out; none : Out }) -> Out;

    let none : fun (A : Type) -> Option A :=
      fun A := fun Out cases := cases.none;
    let some : fun (A : Type) -> A -> Option A :=
      fun A a := fun Out cases := cases.some a;

    let Eq := {
      T : Type;
      eq : T -> T -> Bool;
    };

    let Map := {
      Key : Type;
      Map : Type -> Type;
      empty : fun (A : Type) -> Map A;
      add : fun (A : Type) -> Key -> A -> Map A -> Map A;
      lookup : fun (A : Type) -> Key -> Map A -> Option A;
    };

    -- TODO: sealing operator
    let eq-map : fun (key : Eq) -> Map [ Key := key.T ] :=
      fun key := {
        Map := fun A := key.T -> Option A
        empty := fun A := fun x := none A;
        add := fun A k v map :=
          fun x := (key.eq x k) (Option A) {
            true := some A v;
            false := map x;
          };
        lookup := fun A k map := map k;
      };

    Type
  *)
  let map_functor =
    Let ("Bool", None, FunType (["Out", Univ; "cases", RecType ["true", Name "Out"; "false", Name "Out"]], Name "Out"),
    Let ("true", Some (Name "Bool"), FunLit (["Out"; "cases"], Proj (Name "cases", ["true"])),
    Let ("false", Some (Name "Bool"), FunLit (["Out"; "cases"], Proj (Name "cases", ["false"])),

    Let ("Option", Some (FunArrow (Univ, Univ)),
      FunLit (["A"], FunType (["Out", Univ; "cases", RecType ["some", FunArrow (Name "A", Name "Out"); "none", Name "Out"]], Name "Out")),
    Let ("none", Some (FunType (["A", Univ], App (Name "Option", [Name "A"]))),
      FunLit (["A"], FunLit (["Out"; "cases"], Proj (Name "cases", ["none"]))),
    Let ("some", Some (FunType (["A", Univ], FunArrow (Name "A", App (Name "Option", [Name "A"])))),
      FunLit (["A"; "a"], FunLit (["Out"; "cases"], App (Proj (Name "cases", ["some"]), [Name "a"]))),

    Let ("Eq", None,
      RecType ["T", Univ; "eq", FunArrow (Name "T", FunArrow (Name "T", Name "Bool"))],
    Let ("Map", None,
      RecType [
        "Key", Univ;
        "Map", FunArrow (Univ, Univ);
        "empty", FunType (["A", Univ], App (Name "Map", [Name "A"]));
        "add", FunType (["A", Univ],
          FunArrow (Name "Key", FunArrow (Name "A",
            FunArrow (App (Name "Map", [Name "A"]), App (Name "Map", [Name "A"])))));
        "lookup", FunType (["A", Univ],
          FunArrow (Name "Key",
            FunArrow (App (Name "Map", [Name "A"]), App (Name "Option", [Name "A"]))));
      ],
    Let ("eq-map", Some (FunType (["key", Name "Eq"], Patch (Name "Map", ["Key", Proj (Name "key", ["T"])]))),
      FunLit (["key"],
        RecLit [
          "Map", FunLit (["A"], FunArrow (Proj (Name "key", ["T"]), App (Name "Option", [Name "A"])));
          "empty", FunLit (["A"], FunLit (["k"], App (Name "none", [Name "A"])));
          "add", FunLit (["A"; "k"; "v"; "map"],
            FunLit (["x"], App (App (Proj (Name "key", ["eq"]), [Name "x"; Name "k"]), [
              App (Name "Option", [Name "A"]);
              RecLit [
                "true", App (Name "some", [Name "A"; Name "v"]);
                "false", App (Name "none", [Name "A"]);
              ];
            ])));
          "lookup", FunLit (["A"; "k"; "map"], App (Name "map", [Name "k"]));
        ]),
      Univ)))))))))

  (*
    let category := {
      Ob : Type;
      Hom : { s : Ob; t : Ob } -> Type;
      id : fun (x : Ob) -> Hom [ s := x; t := x ];
      seq : fun (f : Hom) (g : Hom [ s := f.t }) -> Hom [ s := f.s; t := g.t ];
    };
  *)
  let category_ty =
    RecType [
      "Ob", Univ;
      "Hom", FunArrow (RecType ["s", Name "Ob"; "t", Name "Ob"], Univ);
      "id", FunType (["x", Name "Ob"], Patch (Name "Hom", ["s", Name "x"; "t", Name "x"]));
      "seq", FunType (["f", Name "Hom"; "g", Patch (Name "Hom", ["s", Proj (Name "f", ["t"])])],
        Patch (Name "Hom", ["s", Proj (Name "f", ["s"]); "t", Proj (Name "g", ["t"])]));
    ]

  (*
    let types : category := {
      Ob := Type;
      Hom := fun params := params.s -> params.t;
      id := fun A a := a;
      seq := fun f g a := g (f a);
    };
  *)
  let types_tm =
    Let ("category", None, category_ty,
      Ann (
        RecLit [
          "Ob", Univ;
          "Hom", FunLit (["params"], FunArrow (Proj (Name "params", ["s"]), Proj (Name "params", ["t"])));
          "id", FunLit (["A"; "a"], Name "a");
          "seq", FunLit (["f"; "g"; "a"], App (Name "g", [Name "f"; Name "a"]));
        ],
        Name "category"))

  let terms = [
    "fun_record_ty", fun_record_ty;
    "record_ty_patch1", record_ty_patch1;
    "record_ty_patch2", record_ty_patch2;
    "record_ty_patch3", record_ty_patch3;
    "record_ty_patch3b", record_ty_patch3b;
    "record_ty_patch4", record_ty_patch4;
    "record_ty_patch5", record_ty_patch5;
    "record_ty_patch6", record_ty_patch6;
    "record_lit_missing1", record_lit_missing1;
    "record_lit_missing2", record_lit_missing2;
    "record_lit_missing3", record_lit_missing3;
    "record_lit_coerce1", record_lit_coerce1;
    "record_lit_coerce2", record_lit_coerce2;
    "record_lit_coerce_missing1", record_lit_coerce_missing1;
    "record_lit_coerce_missing2", record_lit_coerce_missing2;
    "intro_sing", intro_sing;
    "elim_sing", elim_sing;
    "sing_tm1", sing_tm1;
    "let_ann_check", let_ann_check;
    "let_ann_infer", let_ann_infer;
    "map_functor", map_functor;
    (* TODO: requires total space conversion like in CoolTT *)
    (* "category_ty", category_ty; *)
    (* "types_tm", types_tm; *)
  ]

end

let () =
  Printexc.record_backtrace true;

  let results = Examples.terms |> List.map
    (fun (name, term) ->
      Format.printf "testing %s:\n" name;
      Format.printf "\n";
      try
        let context = Surface.initial_context in
        let tm, ty = Surface.infer context term in
        Format.printf "  inferred type   │ %s\n" (Surface.pretty_quoted context ty Core.Semantics.Univ);
        Format.printf "  evaluated term  │ %s\n" (Surface.pretty_quoted context (Surface.eval context tm) ty);
        Format.printf "\n";
        Format.printf "  %s ... ok\n" name;
        Format.printf "\n";
        (name, `Passed)
      with e ->
        let msg = Printexc.to_string e in
        let stack = Printexc.get_backtrace () |> String.split_on_char '\n' in
        Format.printf "  caught exception: \n";
        Format.printf "\n";
        List.iter (fun line -> Format.printf "    %s\n" line) (msg :: stack);
        Format.printf "  %s ... FAILED\n" name;
        Format.printf "\n";
        (name, `Failed)
    ) in

  let passed, failed, name_width = results |> List.fold_left
    (fun (passed, failed, width) -> function
      | name, `Passed -> (passed + 1, failed, max (String.length name) width)
      | name, `Failed -> (passed, failed +1, max (String.length name) width))
    (0, 0, 0) in

  Format.printf "\n";
  Format.printf "test summary:\n";
  Format.printf "\n";
  results |> List.iter (function
    | (name, `Passed) -> Format.printf "  %-*s ... ok\n" name_width name
    | (name, `Failed) -> Format.printf "  %-*s ... FAILED\n" name_width name);
  Format.printf "\n";
  Format.printf "test result: %s. %i passed; %i failed\n"
    (if failed = 0 then "ok" else "FAILED") passed failed;

  exit (if failed = 0 then 0 else 1)


(** {1 Syntax bikeshedding}

    {2 Record patching}

    - [ R [ B := A; ... ] ]
    - [ R.{ B := A; ... } ] (better for postfix chaining)
    - [ R # [ B .= A, ... ] ] (like in CoolTT)
    - [ R # { B := A; ... } ]
    - [ R (B := A, ...) ] (possibly overloaded with function application)
    - [ R where B := A, ... ] (like in Standard-ML)

    {2 Singleton types}

    - [ A [ x ] ] (like in mb64’s original implementation)
    - [ A [= x ] ] (riffing on the idea of using square brackets for ‘refinement’)
    - [ A [:= x ] ] (mirrors patching more)
    - [ (= x) ] (like in 1ML)
    - [ (= x : A) ]
    - [ (:= x : A) ]
    - [ A (= x) ]
    - [ A (:= x) ] (parens read like, “btw, it's equal to [ x ]”)
*)
