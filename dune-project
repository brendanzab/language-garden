(lang dune 3.16)

(name language-garden)

(source (github brendanzab/language-garden))
(authors "Brendan Zabarauskas")

(using mdx 0.4)
(using menhir 2.1)
(cram enable)

(implicit_transitive_deps false)
(opam_file_location inside_opam_directory)
(generate_opam_files)

; Compilation

(package
  (name compile-arith)
  (depends
    (alcotest (and (>= "1.8") :with-test))
    (cmdliner (>= "1.3"))
    (mdx (and (>= "2.4.1") :with-test))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (qcheck (and (>= "0.22") :with-test))
    (qcheck-core (and (>= "0.22") :with-test))
    (qcheck-alcotest (and (>= "0.22") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name compile-arithcond)
  (depends
    (alcotest (and (>= "1.8") :with-test))
    (cmdliner (>= "1.3"))
    (mdx (and (>= "2.4.1") :with-test))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (qcheck (and (>= "0.22") :with-test))
    (qcheck-core (and (>= "0.22") :with-test))
    (qcheck-alcotest (and (>= "0.22") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name compile-closure-conv)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))


; Elaboration

(package
  (name elab-dependent)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-dependent-sugar)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-stlc-abstract)
  (depends
    ; (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    ; (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-stlc-bidirectional)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-stlc-bidirectional-stratify)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-stlc-letrec-unification)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-stlc-unification)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-stlc-variant-unification)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-system-f-bidirectional)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name elab-record-patching)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))


; Languages

(package
  (name lang-datalog)
  (depends
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name lang-declarative-graphics)
  (depends
    (js_of_ocaml-compiler (>= "5.8.2"))
    (js_of_ocaml (>= "5.8.2"))
    (js_of_ocaml-ppx (>= "5.8.2"))))

(package
  (name lang-doc-templates)
  (depends
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name lang-fractal-growth)
  (depends
    (cmdliner (>= "1.3"))
    (mdx (and (>= "2.4.1") :with-test))))

(package
  (name lang-fractal-tree-rewriting)
  (allow_empty)
  (depends
    lang-declarative-graphics
    (js_of_ocaml-compiler (>= "5.8.2"))
    (js_of_ocaml (>= "5.8.2"))
    (js_of_ocaml-ppx (>= "5.8.2"))))

(package
  (name lang-lc-interpreters)
  (depends
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (sedlex (>= "3.3"))))

(package
  (name lang-shader-graphics)
  (depends
    (cmdliner (>= "1.3"))
    (mdx (and (>= "2.4.1") :with-test))))


; Work in progress projects

(package
  (name wip-compile-stlc)
  (depends
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))
    (sedlex (>= "3.3"))))

(package
  (name wip-compile-stratify))

(package
  (name wip-compile-uncurry))

(package
  (name wip-elab-builtins)
  (depends
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (mdx (and (>= "2.4.1") :with-test))))
