(lang dune 3.0)

(name language-garden)

(using menhir 2.1)
(cram enable)

(implicit_transitive_deps false)
(generate_opam_files true)


(source (github brendanzab/language-garden))
(license Apache-2.0)
(authors "Brendan Zabarauskas")


(package
  (name compile-arith)
  (depends
    (menhir :build)
    (alcotest (and (>= 1.5.0) :with-test))
    (cmdliner (and (>= v1.1.1) :with-test))
    (qcheck (and (>= 0.19.1) :with-test))
    (qcheck-core (and (>= 0.19.1) :with-test))
    (qcheck-alcotest (and (>= 0.19.1) :with-test))))

(package
  (name compile-arithcond)
  (depends
    (menhir :build)
    (alcotest (and (>= 1.5.0) :with-test))
    (cmdliner (and (>= v1.1.1) :with-test))
    (qcheck (and (>= 0.19.1) :with-test))
    (qcheck-core (and (>= 0.19.1) :with-test))
    (qcheck-alcotest (and (>= 0.19.1) :with-test))))

(package
  (name compile-uncurry))

(package
  (name compile-sdf))


(package
  (name elab-dependent)
  (depends
    (menhir :build)))

(package
  (name elab-record-patching))


(package
  (name old-elab-dependent)
  (depends
    (menhir :build)
    (pp (and (>= 1.1.2)))
    (ppx_string (and (>= v0.15.0) :with-test))))
