# A comparison of name binding techniques

This project is a comparison of name binding techniques as applied to
tree-walking interpreters for the simply typed lambda calculus.

## Overview

### Term rewriting based interpreters

| Module                      | Variables          |
| --------------------------- | ------------------ |
| [`Named`]                   | Strings            |
| [`Nameless`]                | Indices            |
| [`Unique`]                  | Unique Ids         |
| [`LocallyNameless`] (TODO)  | Strings + Indices  |

These interpreters apply rewrites directly to the syntax. On the surface this
seems like a straightforward approach, but requires a great amount of care to be
taken to ensure that substitutions are capture-avoiding.

### Normalisation-by-evaluation (NbE) based interpreters

| Module                      | Variables (Syntax) | Variables (Semantics)  | Closures (Semantics)    |
| --------------------------- | ------------------ | ---------------------- | ----------------------- |
| [`NamedClosures`]           | Strings            | Strings                | Defunctionalised        |
| [`NamedHoas`]               | Strings            | Strings                | Host functions          |
| [`NamelessClosures`]        | Indices            | Levels                 | Defunctionalised        |
| [`NamelessHoas`]            | Indices            | Levels                 | Host functions          |
| [`UniqueClosures`]          | Unique Ids         | Unique Ids             | Defunctionalised        |
| [`UniqueHoas`]              | Unique Ids         | Unique Ids             | Host functions          |

Normalisation-by-evaluation breaks up normalisation into evaluation and
quotation. This makes capture-avoidance much more straightforward and efficient,
as variables only need to be renamed once during quotation.

## Discussion

I personally prefer [`NamelessClosures`] and [`NamelessHoas`] for most things.
Normalisation-by-evaluation (NbE) in particular is pretty useful because using
separate types for the syntax and semantics can help to ensure you don’t forget
to fully evaluate terms. It also avoids expensive shifting when using de Bruijn
indices (this is done in a single pass during quotation), and the delicacy of
implementing capture-avoiding substitution when using names.

[`Named`]:              ./Named.ml
[`NamedClosures`]:      ./NamedClosures.ml
[`NamedHoas`]:          ./NamedHoas.ml
[`Nameless`]:           ./Nameless.ml
[`NamelessClosures`]:   ./NamelessClosures.ml
[`NamelessHoas`]:       ./NamelessHoas.ml
[`Unique`]:             ./Unique.ml
[`UniqueClosures`]:     ./UniqueClosures.ml
[`UniqueHoas`]:         ./UniqueHoas.ml
[`LocallyNameless`]:    ./LocallyNameless.ml

## Resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/tree/master)
- Benjamin C. Pierce, “Types and Programming Languages” ([URL](https://www.cis.upenn.edu/~bcpierce/tapl/))
- Lennart Augustsson, “λ-calculus cooked four ways” ([PDF](https://github.com/mietek/cook/blob/master/doc/pdf/augustsson-2006.pdf))
- [sweirich/lambda-n-ways](https://github.com/sweirich/lambda-n-ways/)
