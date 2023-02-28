# Closure Conversion

Implementations of typed closure conversion for a simply typed lambda calculus.

Closure conversion makes implicit variable captures explicit by translating
anonymous functions into closures that contain the _code_ of the original
function and an _environment_ of the captured variables.

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
Translation.FunToClos     Translation.FunToFunA
           │                         │
           ▼                         ▼
     ┌───────────┐             ┌───────────┐
     │ Lang.Clos │             │ Lang.FunA │
     └───────────┘             └───────────┘
                                     │
                          Translation.FunAToClosA
                                     │
                                     ▼
                               ┌────────────┐
                               │ Lang.ClosA │
                               └────────────┘

```

| Language          | Description                             |
| ----------------- | --------------------------------------- |
| [`Lang.Fun`]      | Simply typed lambda calculus            |
| [`Lang.Clos`]     | Closure converted functional language   |
| [`Lang.FunA`]     | Alpha-renamed version of [`Lang.Fun`]   |
| [`Lang.ClosA`]    | Alpha-renamed version of [`Lang.Clos`]  |

[`Lang.Fun`]: ./lib/Lang_Fun.ml
[`Lang.Clos`]: ./lib/Lang_Clos.ml
[`Lang.FunA`]: ./lib/Lang_FunA.ml
[`Lang.ClosA`]: ./lib/Lang_ClosA.ml

| Translation                 | Source          | Target         | Description
| --------------------------- | --------------- | -------------- | ---------------------------------
| [`Translation.FunToFunA`]   | [`Lang.Fun`]    | [`Lang.FunA`]  | Alpha renaming translation
| [`Translation.FunToClos`]   | [`Lang.Fun`]    | [`Lang.Clos`]  | Typed closure conversion
| [`Translation.FunAToClosA`] | [`Lang.FunA`]   | [`Lang.ClosA`] | Typed closure conversion (alpha renamed)

[`Translation.FunToFunA`]: ./lib/Translation_FunToFunA.ml
[`Translation.FunToClos`]: ./lib/Translation_FunToClos.ml
[`Translation.FunAToClosA`]: ./lib/Translation_FunAToClosA.ml

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

<!-- $MDX file=test/multiple-captures-3.stdout -->
```text
let a : Int := 2;
let b : Int := 5;
let f : Int -> Int -> Int :=
  clos(fun (env : (Int, Int)) (x : Int) =>
       clos(fun (env : (Int, Int, Int)) (y : Int) =>
            env.0 * env.2 + env.1 * y,
         (env.0, env.1, x)),
    (a, b));
f 7 3
```

## Resources

- “Typed closure conversion” <https://doi.org/10.1145/237721.237791>
- “A type-preserving closure conversion in haskell” <https://doi.org/10.1145/1291201.1291212>

## Future work

- [x] Closure conversion on nameless terms
- [x] Avoid shifting during translation with de Bruijn levels
- [x] Alpha renaming translation
- [x] Closure conversion on alpha renamed terms
- [ ] Lambda lifting
- [ ] Parameter list flattening
- [ ] Recursive functions
- [ ] Property based tests
- [ ] Dependently typed closure conversion. See:
  - “Typed closure conversion for the calculus of constructions” <https://doi.org/10.1145/3192366.3192372>
  - “Compiling with Dependent Types” <https://www.williamjbowman.com/resources/wjb-dissertation.pdf>
