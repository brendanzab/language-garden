# Arithmetic expression compiler

An interpreter and compiler for a language of arithmetic expressions.

The correctness of compilation and pretty printing are tested with
[property-based tests](./test/properties.ml) implemented using the [qcheck]
library.

## Compiler overview

| Language       | Description                  |
| -------------- | ---------------------------- |
| [`Tree_lang`]  | Arithmetic expressions as a tree of nested subexpressions |
| [`Anf_lang`]   | Arithmetic expressions in A-Normal Form |
| [`Stack_lang`] | Arithmetic expressions as stack machine instructions |

[`Tree_lang`]: ./lib/Tree_lang.ml
[`Anf_lang`]: ./lib/Anf_lang.ml
[`Stack_lang`]: ./lib/Stack_lang.ml

| Translation       |   | Source        |   | Target         |
| ----------------- | - | ------------- | - | -------------- |
| [`Tree_to_anf`]   | : | [`Tree_lang`] | → | [`Anf_lang`]   |
| [`Tree_to_stack`] | : | [`Tree_lang`] | → | [`Stack_lang`] |

[`Tree_to_anf`]: ./lib/tree_to_anf.ml
[`Tree_to_stack`]: ./lib/tree_to_stack.ml

## Compilation Targets

The compiler currently targets the following intermediate languages:

### Stack machine instructions

This is similar to what can be found in stack based languages like [Forth] and
[Java bytecode]:

```sh
$ arith compile --target=stack <<< "1 + -2 * 7"
int 1;
int 2;
neg;
int 7;
mul;
add;
```

### A-Normal Form

This defines an intermediate binding for each computation. This is close to
the [three-address code] found in many optimising compilers.

```sh
$ arith compile --target=anf <<< "1 + -2 * 7"
let e0 := neg 2;
let e1 := mul e0 7;
add 1 e1
```

### A-Normal Form

This defines an intermediate binding for each computation. This is close to
the [three-address code] found in many optimising compilers.

```sh
$ arith compile --target=anf <<< "1 + -2 * 7"
let e0 := neg 2;
let e1 := mul e0 7;
add 1 e1
```

This implementation is extended with conditionals and let expressions in the
[compile-arithcond](../compile-arithcond) project.

[Forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)
[Java bytecode]: https://en.wikipedia.org/wiki/Java_bytecode
[three-address code]: https://en.wikipedia.org/wiki/Three-address_code
[qcheck]: https://github.com/c-cube/qcheck

## Resources

### Stack Machines

- “Efficiently Implementing the Lambda Calculus With Zinc”, Andre Popovitch 2021
  [[URL](https://blog.andrepopovitch.com/zinc/)]
- “Functional programming languages, Part II: abstract machines”, Xavier Leroy 2015
  [[Slides](https://xavierleroy.org/mpri/2-4/machines.pdf)]
- “From Krivine’s machine to the Caml implementations”, Xavier Leroy 2005
  [[Slides](https://xavierleroy.org/talks/zam-kazam05.pdf)]

### A-Normal Form

- “A-Normalization: Why and How”, Matt Might
  [[URL](https://matt.might.net/articles/a-normalization/)]
- “The essence of compiling with continuations”, Flanagan et al. 1993
  [[DOI](https://doi.org/10.1145/173262.155113)]

### Property based testing for compilers

- “Effect-Driven QuickChecking of Compilers“, Midtgaard et al. 2017
  [[DOI](https://doi.org/10.1145/3110259)]
  [[Github](https://github.com/jmid/efftester/)]
