# A comparison of name binding techniques

This project is a comparison of name binding techniques as applied to
tree-walking interpreters for the simply typed lambda calculus.

## Overview

### Term rewriting based interpreters

| Module                      | Variables (Syntax)           |
| --------------------------- | ---------------------------- |
| [`Named`]                   | Strings                      |
| [`Nameless`]                | De Bruijn indices            |
| [`Unique`]                  | Unique identifiers           |
| [`LocallyNameless`] (TODO)  | Strings + De Bruijn indices  |

These interpreters apply substitutions directly to the syntax. This might seems
on the surface like a straightforward approach, but a great amount of care needs
to be taken to ensure that substitutions are capture-avoiding.

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
quotation. This makes capture-avoidance much more straightforward and efficient,
as variables only need to be renamed once during quotation.
In the case of named and unique variables, fresh names are generated as
binders are encountered.
For de Bruijn indices, quotation handles the shifting of variables in a single
pass, using the size of the environment to convert from levels back to indices.

## Discussion

I personally prefer [`NamelessClosures`] and [`NamelessHoas`] for most things.
NbE is nice because using separate types for the syntax and semantics helps to
ensure you don’t forget to fully evaluate terms.
As mentioned before, it also avoids expensive shifting when using de Bruijn
indices, and avoids the delicacy of implementing capture-avoiding substitution
when using names.

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
