(lang dune 3.0)

(name language-garden)

(using mdx 0.2)
(using menhir 2.1)
(cram enable)

(implicit_transitive_deps false)
(generate_opam_files true)


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


; Elaboration

(package
  (name elab-dependent)
  (depends
    (cmdliner (>= v1.1.1))
    (menhir :build)))

(package
  (name elab-record-patching))


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


; Old projects

(package
  (name old-elab-dependent)
  (depends
    (menhir :build)
    (pp (>= 1.1.2))
    (ppx_string (and (>= v0.15.0) :with-test))))
