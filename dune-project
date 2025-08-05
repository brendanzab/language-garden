(lang dune 3.16)

(name language-garden)

(source (github brendanzab/language-garden))
(authors "Brendan Zabarauskas")
(license MIT)

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
  (alcotest (and (>= "1.9") :with-test))
  (cmdliner (>= "1.3"))
  (mdx (and (>= "2.5") :with-test))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (qcheck (and (>= "0.25") :with-test))
  (qcheck-core (and (>= "0.25") :with-test))
  (qcheck-alcotest (and (>= "0.25") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name compile-arithcond)
 (depends
  (alcotest (and (>= "1.9") :with-test))
  (cmdliner (>= "1.3"))
  (mdx (and (>= "2.5") :with-test))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (qcheck (and (>= "0.25") :with-test))
  (qcheck-core (and (>= "0.25") :with-test))
  (qcheck-alcotest (and (>= "0.25") :with-test))
  (sedlex (>= "3.6"))))


; Elaboration

(package
 (name elab-dependent)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-dependent-sugar)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-abstract)
 (depends
  ; (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  ; (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-bidirectional)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-error-recovery)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-bidirectional-stratify)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-letrec-unification)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-unification)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-stlc-row-unification)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-system-f-bidirectional)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-system-f-unification)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name elab-record-patching)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))


; Languages

(package
 (name lang-datalog)
 (depends
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name lang-declarative-graphics)
 (depends
  (js_of_ocaml-compiler (>= "6.0.1"))
  (js_of_ocaml (>= "6.0.1"))
  (js_of_ocaml-ppx (>= "6.0.1"))))

(package
 (name lang-doc-templates)
 (depends
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name lang-fractal-growth)
 (depends
  (cmdliner (>= "1.3"))
  (mdx (and (>= "2.5") :with-test))))

(package
 (name lang-fractal-tree-rewriting)
 (allow_empty)
 (depends
  lang-declarative-graphics
  (js_of_ocaml-compiler (and (>= "6.0.1") (< "6.1.0")))
  (js_of_ocaml (and (>= "6.0.1") (< "6.1.0")))
  (js_of_ocaml-ppx (and (>= "6.0.1") (< "6.1.0")))))

(package
 (name lang-lc-interpreters)
 (depends
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (sedlex (>= "3.6"))))

(package
 (name lang-shader-graphics)
 (depends
  (cmdliner (>= "1.3"))
  (mdx (and (>= "2.5") :with-test))))


; Work in progress projects

(package
 (name wip-compile-closure-conv)
 (depends
  (cmdliner (>= "1.3"))
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

(package
 (name wip-compile-stlc)
 (depends
  (menhir (and (>= "20240715") :build))
  (menhirLib (>= "20240715"))
  (mdx (and (>= "2.5") :with-test))
  (sedlex (>= "3.6"))))

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
  (mdx (and (>= "2.5") :with-test))))
