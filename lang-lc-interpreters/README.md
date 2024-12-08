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

| Module                        | Variables (Syntax) | Variables (Semantics)  | Closures (Semantics)    |
| ----------------------------- | ------------------ | ---------------------- | ----------------------- |
| [`Named_nbe_closures`]        | Strings            | Strings                | Defunctionalised        |
| [`Named_nbe_hoas`]            | Strings            | Strings                | Host functions          |
| [`Nameless_nbe_closures`]     | De Bruijn indices  | De Bruijn levels       | Defunctionalised        |
| [`Nameless_nbe_hoas`]         | De Bruijn indices  | De Bruijn levels       | Host functions          |
| [`Unique_nbe_closures`]       | Unique identifiers | Unique identifiers     | Defunctionalised        |
| [`Unique_nbe_hoas`]           | Unique identifiers | Unique identifiers     | Host functions          |

Normalisation-by-evaluation (NbE) breaks up normalisation into evaluation and
quotation.
In NbE, a separate datatype is used for values, which can be helpful for
ensuring you don’t forget to fully evaluate terms – the types guide you to a
correct implementation.
It also makes capture-avoidance much more straightforward and efficient,
as variables only need to be renamed or shifted in a single pass during
quotation.

## Todo

Infrastructure:

- [ ] Example based tests
- [ ] Property based tests
- [ ] Benchmarks

Variable representations:

- [x] Named
- [x] Unique ids
- [x] De Bruijn indices
- [ ] De Bruijn levels
- [ ] Co-de Bruijn
- [ ] Locally nameless
- [ ] NbE with De Bruijn indices and unique IDs (like in [Sixty](https://github.com/ollef/sixty/))
- [ ] Using [Bindlib](https://ocaml.org/p/bindlib/)

## Discussion

For implementations I personally prefer [`Nameless_nbe_closures`] or
[`Nameless_nbe_hoas`] for most things, as it provides a good balance of convenience,
ease of implementation, and performance. Other constraints might tip the balance
however, for example unique identifiers can make elaborating pattern matching to
case trees more straightforward.

[`Named`]:                  ./named.ml
[`Named_nbe_closures`]:     ./named_nbe_closures.ml
[`Named_nbe_hoas`]:         ./named_nbe_hoas.ml
[`Nameless`]:               ./nameless.ml
[`Nameless_nbe_closures`]:  ./nameless_nbe_closures.ml
[`Nameless_nbe_hoas`]:      ./nameless_nbe_hoas.ml
[`Unique`]:                 ./unique.ml
[`Unique_nbe_closures`]:    ./unique_nbe_closures.ml
[`Unique_nbe_hoas`]:        ./unique_nbe_hoas.ml

## Resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/tree/master)
- Benjamin C. Pierce, “Types and Programming Languages” ([URL](https://www.cis.upenn.edu/~bcpierce/tapl/))
- Lennart Augustsson, “λ-calculus cooked four ways” ([PDF](https://github.com/mietek/cook/blob/master/doc/pdf/augustsson-2006.pdf))
- [sweirich/lambda-n-ways](https://github.com/sweirich/lambda-n-ways/)
