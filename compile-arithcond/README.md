# Arithmetic expression compiler

An interpreter and compiler for a language of arithmetic expressions, extended
with booleans, let bindings and conditional expressions.

The correctness of type checking, compilation and pretty printing are tested
with [property-based tests](./test/Properties.ml) implemented using the [qcheck]
library.

## Compiler overview

| Language      | Description                  |
| ------------- | ---------------------------- |
| [`TreeLang`]  | Arithmetic expressions as a tree of nested subexpressions |
| [`AnfLang`]   | Arithmetic expressions in A-Normal Form |
| [`StackLang`] | Arithmetic expressions as stack machine instructions |

[`TreeLang`]: ./lib/TreeLang.ml
[`AnfLang`]: ./lib/AnfLang.ml
[`StackLang`]: ./lib/StackLang.ml

| Source       | Translation     | Target        |
| :----------: | :-------------: | :-----------: |
| [`TreeLang`] | [`TreeToAnf`]   | [`AnfLang`]   |
| [`TreeLang`] | [`TreeToStack`] | [`StackLang`] |

[`TreeToAnf`]: ./lib/TreeToAnf.ml
[`TreeToStack`]: ./lib/TreeToStack.ml

## Compilation Targets

The compiler currently targets the following intermediate languages:

### Stack machine instructions

This is similar to what can be found in stack based languages like [Forth] and
[Java bytecode]:

```sh
$ arithcond compile --target=stack <<< "let x := 3 * 4; if x = 5 then (let y := 3 + x; 8 - y / 4) else x + 8"
int 3;
int 4;
mul;
begin-let;
access 0;
int 5;
eq;
code [ int 3; access 0; add; begin-let; int 8; access 0; int 4; div; sub;
     end-let; ];
code [ access 0; int 8; add; ];
if;
end-let;
```

### A-Normal Form

This defines an intermediate binding for each computation. This is close to
the [three-address code] found in many optimising compilers.

```sh
$ arithcond compile --target=anf <<< "let x := 3 * 4; if x = 5 then (let y := 3 + x; 8 - y / 4) else x + 8"
let e0 := mul 3 4;
let e1 := eq e0 5;
if e1 then
  let e2 := add 3 e0;
  let e3 := div e2 4;
  sub 8 e3
else
  add e0 8
```

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
