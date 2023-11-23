# A comparison of name binding techniques

This project is a comparison of name binding techniques as applied to
tree-walking interpreters for the simply typed lambda calculus.

| Module                      | Interpreter style | Expressions     | Values                  |
| --------------------------- | ----------------- | --------------- | ----------------------- |
| [`Named`]                   | Term Rewriting    | Names           | Names                   |
| [`NamedClosures`]           | NbE               | Names           | Names + Closures        |
| [`NamedHoas`]               | NbE               | Names           | Levels + Host functions |
| [`Nameless`]                | Term Rewriting    | Indices         | Indices                 |
| [`NamelessClosures`]        | NbE               | Indices         | Levels + Closures       |
| [`NamelessHoas`]            | NbE               | Indices         | Levels + Host functions |
| [`LocallyNameless`] (TODO)  | Term Rewriting    | Names + Indices | Names + Indices         |

I personally prefer [`NamelessClosures`] and [`NamelessHoas`] for most things.
NbE in particular is pretty useful as using separate types for the syntax and
semantics can help to ensure you don’t forget to fully evaluate terms, and helps
to avoid expensive shifting when using de Bruijn indices (this is done in a
single pass during quotation).

[`Named`]:              ./Named.ml
[`NamedClosures`]:      ./NamedClosures.ml
[`NamedHoas`]:          ./NamedHoas.ml
[`Nameless`]:           ./Nameless.ml
[`NamelessClosures`]:   ./NamelessClosures.ml
[`NamelessHoas`]:       ./NamelessHoas.ml
[`LocallyNameless`]:    ./LocallyNameless.ml

## Resources

- [AndrasKovacs/elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/tree/master)
- Benjamin C. Pierce, “Types and Programming Languages” <https://www.cis.upenn.edu/~bcpierce/tapl/>
