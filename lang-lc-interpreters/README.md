# A comparison of lambda calculus interpreters

This project is a comparison of name binding techniques as applied to
tree-walking interpreters for the simply typed lambda calculus.
Lambda calculus interpreters like these form the basis of more advanced
programming language implementations.

## Overview

### Term rewriting based interpreters

| Module                      | Variables (Syntax)           |
| --------------------------- | ---------------------------- |
| [`Named`]                   | Strings                      |
| [`Nameless`]                | De Bruijn indices            |
| [`Unique`]                  | Unique identifiers           |

These interpreters apply substitutions directly to the syntax.
This might seem straightforward, but a great amount of care needs to be taken to
ensure that substitutions are capture-avoiding.

Both the named and unique identifier approaches to variable representation
require substitutions to be taken into account when checking for alpha
equivalence. In contrast, for De Bruijn indices this is a straightforward
equality comparison.

### Normalisation-by-evaluation based interpreters

| Module                      | Variables (Syntax) | Variables (Semantics)  | Closures (Semantics)    |
| --------------------------- | ------------------ | ---------------------- | ----------------------- |
| [`NamedClosures`]           | Strings            | Strings                | Defunctionalised        |
| [`NamedHoas`]               | Strings            | Strings                | Host functions          |
| [`NamelessClosures`]        | De Bruijn indices  | De Bruijn levels       | Defunctionalised        |
| [`NamelessHoas`]            | De Bruijn indices  | De Bruijn levels       | Host functions          |
| [`UniqueClosures`]          | Unique identifiers | Unique identifiers     | Defunctionalised        |
| [`UniqueHoas`]              | Unique identifiers | Unique identifiers     | Host functions          |

Normalisation-by-evaluation (NbE) breaks up normalisation into evaluation and
quotation.
In NbE, a separate datatype is used for values, which can be helpful for
ensuring you don’t forget to fully evaluate terms – the types guide you to a
correct implementation.
It also makes capture-avoidance much more straightforward and efficient,
as variables only need to be renamed or shifted in a single pass during
quotation.

## Todo

- [x] Named
- [x] Unique ids
- [x] De Bruijn indices
- [ ] De Bruijn levels
- [ ] Co-de Bruijn
- [ ] Locally nameless

## Discussion

For implementations I personally prefer [`NamelessClosures`] or
[`NamelessHoas`] for most things, as it provides a good balance of convenience,
ease of implementation, and performance. Other constraints might tip the balance
however, for example unique identifiers can make elaborating pattern matching to
case trees more straightforward.

[`Named`]:              ./Named.ml
[`NamedClosures`]:      ./NamedClosures.ml
[`NamedHoas`]:          ./NamedHoas.ml
[`Nameless`]:           ./Nameless.ml
[`NamelessClosures`]:   ./NamelessClosures.ml
[`NamelessHoas`]:       ./NamelessHoas.ml
[`Unique`]:             ./Unique.ml
[`UniqueClosures`]:     ./UniqueClosures.ml
[`UniqueHoas`]:         ./UniqueHoas.ml

## Resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/tree/master)
- Benjamin C. Pierce, “Types and Programming Languages” ([URL](https://www.cis.upenn.edu/~bcpierce/tapl/))
- Lennart Augustsson, “λ-calculus cooked four ways” ([PDF](https://github.com/mietek/cook/blob/master/doc/pdf/augustsson-2006.pdf))
- [sweirich/lambda-n-ways](https://github.com/sweirich/lambda-n-ways/)
