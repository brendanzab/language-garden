# Closure Conversion

An implementation of typed closure conversion for a simply typed lambda calculus.

Closure conversion is a translation pass that makes implicit variable captures
explicit, by translating anonymous functions into pairs that contain an
environment of captures along with the code of the original function, rewritten
to access captures from the environment explicitly.

## Compiler overview

| Language     | Description                  |
| ------------ | ---------------------------- |
| [`FunLang`]  | Simply typed lambda calculus |
| [`ClosLang`] | Closure converted functional language |

[`FunLang`]: ./bin/FunLang.ml
[`ClosLang`]: ./bin/ClosLang.ml

| Translation   | Source      |   | Target       |
| ------------- | ----------- | - | ------------ |
| [`FunToClos`] | [`FunLang`] | → | [`ClosLang`] |

[`FunToClos`]: ./bin/FunToClos.ml

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

Closure compiled term:

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

- [x] Closure conversion translation
- [x] Avoid shifting during translation with de Bruijn levels
- [ ] Lambda lifting
- [ ] Parameter list flattening
- [ ] Dependently typed closure conversion. See:
  - “Typed closure conversion for the calculus of constructions” <https://doi.org/10.1145/3192366.3192372>
  - “Compiling with Dependent Types” <https://www.williamjbowman.com/resources/wjb-dissertation.pdf>
