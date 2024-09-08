# Simply typed lambda calculus with unification

Extends [**elab-stlc-unification**](../elab-stlc-unification).

This elaborator introduces structural variant types to the simply typed lambda
calculus. In order to infer types, we introduce variant constraints to unsolved
metavariables, that accumulate maps of labelled cases as the program is checked.

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including normalisation, unification, and pretty printing |

[`Main`]: ./Main.ml
[`Lexer`]: ./Lexer.mll
[`Parser`]: ./Parser.mly
[`Surface`]: ./Surface.ml
[`Core`]: ./Core.ml

## Example

<!-- $MDX file=examples/readme.txt -->
```
let apply x :=
  match x with
  | [incr := x] => x + 1
  | [decr := x] => x - 1
  | [square := x] => x * x
  end;

apply [incr := 1]
```

Elaborated program:

<!-- $MDX file=examples/readme.stdout -->
```
let apply : [decr : Int | incr : Int | square : Int] -> Int :=
  fun (x : [decr : Int | incr : Int | square : Int]) =>
    match x with
    | [decr := x] => #int-sub -x 1
    | [incr := x] => #int-add -x 1
    | [square := x] => #int-mul -x x
    end;
apply ([incr := 1] : [decr : Int | incr : Int | square : Int]) : Int
```

More examples can be found in [`tests.t`](tests.t).
