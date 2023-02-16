# Closure Conversion

An implementation of typed closure conversion for a simply typed lambda calculus.

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

## Examples

```sh
$ closure-conv

FunLang:
  let a : Int := 1;
  let f : Int -> Int := fun (x : Int) => x;
  f 100

ClosLang:
  let a : Int := 1;
  let f : Int -> Int := ⟪fun (env : ()) (x : Int) => x, ⟨⟩⟫;
  f 100


FunLang:
  let a : Int := 1;
  let f : Int -> Int := fun (x : Int) => a;
  f 100

ClosLang:
  let a : Int := 1;
  let f : Int -> Int := ⟪fun (env : (Int)) (x : Int) => env.0, ⟨a⟩⟫;
  f 100


FunLang:
  let a : Int := 1;
  let f : Int -> Int := fun (x : Int) => let y : Int := #add x a; y;
  f 100

ClosLang:
  let a : Int := 1;
  let f : Int -> Int :=
    ⟪fun (env : (Int)) (x : Int) => let y : Int := #add x env.0; y,
      ⟨a⟩⟫;
  f 100


FunLang:
  let x : Int := 1;
  let y : Int := 2;
  let z : Int := 3;
  let f : Int -> Int -> Int := fun (w : Int) => #add (#add x y) w;
  f 100

ClosLang:
  let x : Int := 1;
  let y : Int := 2;
  let z : Int := 3;
  let f : Int -> Int -> Int :=
    ⟪fun (env : (Int, Int)) (w : Int) => #add (#add env.0 env.1) w,
      ⟨x, y⟩⟫;
  f 100


FunLang:
  let a : Int := 2;
  let b : Int := 4;
  let c : Int := 7;
  let d : Int := 8;
  fun (x : Int) => #add (#mul a x) c

ClosLang:
  let a : Int := 2;
  let b : Int := 4;
  let c : Int := 7;
  let d : Int := 8;
  ⟪fun (env : (Int, Int)) (x : Int) => #add (#mul env.0 x) env.1,
    ⟨a, c⟩⟫


FunLang:
  let a : Int := 2;
  let b : Int := 5;
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => #add (#mul a x) (#mul b y);
  f 7 3

ClosLang:
  let a : Int := 2;
  let b : Int := 5;
  let f : Int -> Int -> Int :=
    ⟪fun (env : (Int, Int)) (x : Int) =>
       ⟪fun (env : (Int, Int, Int)) (y : Int) =>
          #add (#mul env.0 env.2) (#mul env.1 y),
         ⟨env.0, env.1, x⟩⟫,
      ⟨a, b⟩⟫;
  f 7 3

```
