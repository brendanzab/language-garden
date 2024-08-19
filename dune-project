(lang dune 3.14)

(name language-garden)

(using mdx 0.4)
(using menhir 2.1)
(cram enable)

(implicit_transitive_deps false)
(opam_file_location inside_opam_directory)

(package (name compile-arith))
(package (name compile-arithcond))
(package (name compile-closure-conv))

(package (name elab-dependent))
(package (name elab-dependent-sugar))
(package (name elab-stlc-abstract))
(package (name elab-stlc-bidirectional))
(package (name elab-stlc-bidirectional-stratify))
(package (name elab-stlc-letrec-unification))
(package (name elab-stlc-unification))
(package (name elab-stlc-variant-unification))
(package (name elab-record-patching))

(package (name lang-datalog))
(package (name lang-doc-templates))
(package (name lang-fractal-growth))
(package (name lang-lc-interpreters))
(package (name lang-shader-graphics))

(package (name wip-compile-stratify))
(package (name wip-compile-uncurry))
(package (name wip-elab-builtins))
