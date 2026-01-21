# Simply typed lambda calculus with recursive let bindings

- Extends [**elab-stlc-unification**](../elab-stlc-unification) (+ mutual recursion)

---

This is an implementation of recursive let bindings for the simply typed lambda
calculus:

<!-- $MDX file=examples/factorial.txt -->
```
let rec fact n :=
  if n = 0 then 1 else n * fact (n - 1);

fact 5
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/factorial.stdout -->
```
let rec {
  fact : Int -> Int :=
    fun (n : Int) =>
      if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1));
};
fact 5 : Int
```

</details>

Mutually recursive functions are elaborated to fixed-points over records of
functions:

<!-- $MDX file=examples/even-odd.txt -->
```
let rec {
  is-even n := if n = 0 then true else is-odd (n - 1);
  is-odd n := if n = 0 then false else is-even (n - 1);
};

is-even 6
```

<details>
<summary>Elaboration output</summary>

<!-- $MDX file=examples/even-odd.stdout -->
```
let rec {
  is-even : Int -> Bool :=
    fun (n : Int) => if #int-eq n 0 then true else is-odd (#int-sub n 1);
  is-odd : Int -> Bool :=
    fun (n : Int) => if #int-eq n 0 then false else is-even (#int-sub n 1);
};
is-even 6 : Bool
```

</details>

## Project overview

| Module        | Description                             |
| ------------- | --------------------------------------- |
| [`Main`]      | Command line interface                  |
| [`Lexer`]     | Lexer for the surface language          |
| [`Parser`]    | Parser for the surface language         |
| [`Surface`]   | Surface language, including elaboration |
| [`Core`]      | Core language, including evaluation, unification, and pretty printing |
| [`Prim`]      | Primitive operations                    |

[`Main`]: ./main.ml
[`Lexer`]: ./lexer.ml
[`Parser`]: ./parser.mly
[`Surface`]: ./surface.ml
[`Core`]: ./core.ml
[`Prim`]: ./prim.ml

## Examples

More examples can be found in [`tests.t`](tests.t).
