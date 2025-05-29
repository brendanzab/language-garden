# Compiling Closures

This project demonstrates various approaches to compiling functions to closures
in a simply typed lambda calculus.

- **Closure conversion**: this translation makes implicit variable captures
  explicit by translating anonymous functions into closures that contain the
  _code_ of the original function and an _environment_ of the captured variables.
- **Lambda lifting**: this translation lifts functions to top level bindings,
  cutting down on the number of closures needed.

Two implementations of closure conversion are provided: one between nameless,
de Bruijn indexed terms, the other between alpha-renamed terms.

## Compiler overview

```text
                   ┌──────────┐
                   │ Lang.Fun │
                   └──────────┘
                        │
           ╭────────────┴────────────╮
           │                         │
Translation.Fun_to_clos     Translation.Fun_to_fun_a
           │                         │
           ▼                         ▼
     ┌───────────┐             ┌───────────┐
     │ Lang.Clos │             │ Lang.Fun_a │
     └───────────┘             └───────────┘
                                     │
                        ╭────────────┴────────────╮
                        │                         │
        Translation.Fun_a_to_clos_a   Translation.Fun_a_to_lifted_a
                        │                         │
                        ▼                         ▼
                  ┌────────────┐          ┌──────────────┐
                  │ Lang.Clos_a │          │ Lang.Lifted_a │
                  └────────────┘          └──────────────┘

```

| Language          | Description                                           |
| ----------------- | ----------------------------------------------------- |
| [`Lang.Fun`]      | Simply typed lambda calculus                          |
| [`Lang.Fun_a`]     | Simply typed lambda calculus (alpha-renamed)          |
| [`Lang.Clos`]     | Closure converted functional language                 |
| [`Lang.Clos_a`]    | Closure converted functional language (alpha-renamed) |
| [`Lang.Lifted_a`]  | Lambda lifted functional language (alpha-renamed)     |

[`Lang.Fun`]: ./lib/lang__fun.ml
[`Lang.Clos`]: ./lib/lang__clos.ml
[`Lang.Fun_a`]: ./lib/lang__fun_a.ml
[`Lang.Clos_a`]: ./lib/lang__clos_a.ml
[`Lang.Lifted_a`]: ./lib/lang__lifted_a.ml

| Translation                       |   | Source          |   | Target            | Description
| --------------------------------- | - | --------------- | - | ----------------- | ---------------------------------
| [`Translation.Fun_to_clos`]       | : | [`Lang.Fun`]    | → | [`Lang.Clos`]     | Closure conversion
| [`Translation.Fun_to_fun_a`]      | : | [`Lang.Fun`]    | → | [`Lang.Fun_a`]    | Alpha renaming
| [`Translation.Fun_a_to_clos_a`]   | : | [`Lang.Fun_a`]  | → | [`Lang.Clos_a`]   | Closure conversion (alpha renamed)
| [`Translation.Fun_a_to_lifted_a`] | : | [`Lang.Fun_a`]  | → | [`Lang.Lifted_a`] | Lambda lifting (alpha renamed)

[`Translation.Fun_to_clos`]: ./lib/translation__fun_to_clos.ml
[`Translation.Fun_to_fun_a`]: ./lib/translation__fun_to_fun_a.ml
[`Translation.Fun_a_to_clos_a`]: ./lib/translation__fun_a_to_clos_a.ml
[`Translation.Fun_a_to_lifted_a`]: ./lib/translation__fun_a_to_lifted_a.ml

An evaluator and type checker is implemented for each intermediate language.
Every translation pass should produce well-typed programs in the target language.

## Example

Source term:

<!-- $MDX file=test/multiple-captures-3.txt -->
```text
let a : Int := 2;
let b : Int := 5;
let f : Int -> Int -> Int :=
  fun (x : Int) => fun (y : Int) =>
    a * x + b * y;

f 7 3
```

Closure converted term:

<!-- $MDX file=test/multiple-captures-3.clos.stdout -->
```text
let a : Int := 2;
let b : Int := 5;
let f : Int -> Int -> Int :=
  clos(
    fun (env : (Int, Int)) (x : Int) =>
      clos(
        fun (env : (Int, Int, Int)) (y : Int) =>
          #add (#mul env.0 env.2) (#mul env.1 y),
        (env.0, env.1, x)
      ),
    (a, b)
  );
f 7 3
```

Lambda lifted term:

<!-- $MDX file=test/multiple-captures-3.lifted.stdout -->
```text
def anon0↑ (env4 : (Int, Int, Int)) (y5 : Int) :=
  #add (#mul env4.0 env4.2) (#mul env4.1 y5);
def f1↑ (env2 : (Int, Int)) (x3 : Int) :=
  clos(anon0↑, (env2.0, env2.1, x3));
let a0 : Int := 2;
let b1 : Int := 5;
f1↑ (a0, b1) 7 3
```

## Resources

- “Typed closure conversion” <https://doi.org/10.1145/237721.237791>
- “A type-preserving closure conversion in haskell” <https://doi.org/10.1145/1291201.1291212>

## Future work

- [x] Closure conversion on nameless terms
- [x] Avoid shifting during translation with de Bruijn levels
- [x] Alpha renaming translation
- [x] Closure conversion on alpha renamed terms
- [x] Lambda lifting
- [ ] Parameter list flattening
- [ ] Recursive functions
- [ ] Promote tests for all targets
- [ ] Property based tests
- [ ] Dependently typed closure conversion. See:
  - “Typed closure conversion for the calculus of constructions” <https://doi.org/10.1145/3192366.3192372>
  - “Compiling with Dependent Types” <https://www.williamjbowman.com/resources/wjb-dissertation.pdf>
