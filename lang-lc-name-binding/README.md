# A comparison of name binding techniques

This project is a comparison of name binding techniques as applied to
tree-walking interpreters for the simply typed lambda calculus.

## Overview

Term rewriting based interpreters:

| Module                      | Variables          |
| --------------------------- | ------------------ |
| [`Named`]                   | Strings            |
| [`Nameless`]                | Indices            |
| [`Unique`]                  | Unique Ids         |
| [`LocallyNameless`] (TODO)  | Strings + Indices  |

Normalisation-by-evaluation (NbE) based interpreters:

| Module                      | Variables (Syntax) | Variables (Semantics)  | Closures                |
| --------------------------- | ------------------ | ---------------------- | ----------------------- |
| [`NamedClosures`]           | Strings            | Strings                | First-order closures    |
| [`NamedHoas`]               | Strings            | Strings                | Host functions          |
| [`NamelessClosures`]        | Indices            | Levels                 | First-order closures    |
| [`NamelessHoas`]            | Indices            | Levels                 | Host functions          |
| [`UniqueClosures`]          | Unique Ids         | Unique Ids             | First-order closures    |

## Discussion

I personally prefer [`NamelessClosures`] and [`NamelessHoas`] for most things.
Normalisation-by-evaluation (NbE) in particular is pretty useful because using
separate types for the syntax and semantics can help to ensure you don’t forget
to fully evaluate terms (this is very important in dependent type checkers!). It
also avoids expensive shifting when using de Bruijn indices (this is done in a
single pass during quotation), and the delicacy of implementing capture-avoiding
substitution when using names.

[`Named`]:              ./Named.ml
[`NamedClosures`]:      ./NamedClosures.ml
[`NamedHoas`]:          ./NamedHoas.ml
[`Nameless`]:           ./Nameless.ml
[`NamelessClosures`]:   ./NamelessClosures.ml
[`NamelessHoas`]:       ./NamelessHoas.ml
[`Unique`]:             ./Unique.ml
[`UniqueClosures`]:     ./UniqueClosures.ml
[`LocallyNameless`]:    ./LocallyNameless.ml

## Resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/tree/master)
- Benjamin C. Pierce, “Types and Programming Languages” ([URL](https://www.cis.upenn.edu/~bcpierce/tapl/))
- Lennart Augustsson, “λ-calculus cooked four ways” ([PDF](https://github.com/mietek/cook/blob/master/doc/pdf/augustsson-2006.pdf))
- [sweirich/lambda-n-ways](https://github.com/sweirich/lambda-n-ways/)
