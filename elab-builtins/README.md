# Elaboration with built-in types and operations

> NOTE: A work in progress!

An experiment into implementing elaboration with built-in types and operations.

Adding built-ins/primitives to a language is a common source of frustration and
confusion. It’s easy to fall into cycles of trying one approach, and then
another, and failing to make progress. The goal of this project is to attempt to
explore this design problem in a more isolated fashion.

## Todo list

- [x] initial hard-coded primitives
- [ ] operations
- [ ] glued eval for nicer errors
- [ ] figure out nomenclature: built-ins, primitives, lang-items, pervasives, externs
- [ ] declare literals
- [ ] declare operators
- [ ] external types and operations

## Resources and inspiration

- [Agda Language Reference: Built-ins](https://agda.readthedocs.io/en/latest/language/built-ins.html)
- [Agda Language Reference: Postulates](https://agda.readthedocs.io/en/latest/language/postulates.html)
- [Rust Unstable Book: Lang items](https://doc.rust-lang.org/beta/unstable-book/language-features/lang-items.html)
- [F*'s `primitive_step` type](https://github.com/FStarLang/FStar/blob/60b3e5a4d382406e99529950927cbcfbbd5f310b/src/typechecker/FStar.TypeChecker.Cfg.fsti#L89-L98)
