(lang dune 3.8)

(name language-garden)

(using mdx 0.2)
(using menhir 2.1)
(cram enable)

(implicit_transitive_deps false)
(generate_opam_files true)
(opam_file_location inside_opam_directory)


(source (github brendanzab/language-garden))
(license Apache-2.0)
(authors "Brendan Zabarauskas")


; Compilation

(package
  (name compile-arith)
  (depends
    (alcotest (and (>= 1.5.0) :with-test))
    (cmdliner (>= v1.1.1))
    (menhir :build)
    (qcheck (and (>= 0.19.1) :with-test))
    (qcheck-core (and (>= 0.19.1) :with-test))
    (qcheck-alcotest (and (>= 0.19.1) :with-test))))

(package
  (name compile-arithcond)
  (depends
    (alcotest (and (>= 1.5.0) :with-test))
    (cmdliner (>= v1.1.1))
    (menhir :build)
    (qcheck (and (>= 0.19.1) :with-test))
    (qcheck-core (and (>= 0.19.1) :with-test))
    (qcheck-alcotest (and (>= 0.19.1) :with-test))))

(package
  (name compile-closure-conv)
  (depends
    (cmdliner (>= v1.1.1))
    (menhir :build)))

; Elaboration

(package
  (name elab-dependent)
  (depends
    (cmdliner (>= v1.1.1))
    (menhir :build)))

(package
  (name elab-stlc-unification)
  (depends
    (menhir :build)))

(package
  (name elab-record-patching)
  (depends
    (cmdliner (>= v1.1.1))
    (menhir :build)))


; Languages

(package
  (name lang-fractal-growth)
  (depends
    (cmdliner (>= v1.1.1))))

(package
  (name lang-shader-graphics)
  (depends
    (cmdliner (>= v1.1.1))))


; WIP projects

(package
  (name wip-compile-stratify))

(package
  (name wip-compile-uncurry))

(package
  (name wip-elab-builtins)
  (depends
    (cmdliner (>= v1.1.1))
    (menhir :build)))
