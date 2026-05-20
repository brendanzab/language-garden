# Compiling a first-order functional language

- Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional) (+ top-level items, compilation) (- higher-order functions, currying)
- Based on [**scraps/compile_fofl_wasm.ml**](../scraps/compile_fofl_wasm.ml)

---

A demonstration of compiling a language with top-level, mutually recursive
definitions to Web Assembly. This is a stepping stone towards compiling
higher-order, curried functional languages (like STLC or System-F), avoiding
the complexities of closure conversion, uncurrying, and memory allocation for
now.

## Example

<!-- $MDX file=examples/readme.txt -->
```
val test-fact : I32 :=
  fact(5);

fun fact(n : I32) : I32 :=
  if n = 0 then 1 else n * fact(n - 1);

fun ackermann(m : I32, n : I32) : I32 :=
  if m = 0 then
    n + 1
  else if n = 0 then
    ackermann(m - 1, 1)
  else
    ackermann(m - 1, ackermann(m, n - 1));

fun is-even(n : I32) : Bool :=
  if n = 0 then true else is-odd(n - 1);

fun is-odd(n : I32) : Bool :=
  if n = 0 then false else is-even(n - 1);
```

<details>
<summary>Compiled web assembly</summary>

<!-- $MDX file=examples/readme.wat -->
```wat
(module
  (func
    $ackermann0
    (export "ackermann")
    (param $m0 i32)
    (param $n1 i32)
    (result i32)
    (local.get $m0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $n1) (i32.const 1) i32.add)
      (else
        (local.get $n1)
        (i32.const 0)
        i32.eq
        (if
          (result i32)
          (then
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (i32.const 1)
            (call $ackermann0))
          (else
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (local.get $m0)
            (local.get $n1)
            (i32.const 1)
            i32.sub
            (call $ackermann0)
            (call $ackermann0))))))
  (func
    $fact1
    (export "fact")
    (param $n2 i32)
    (result i32)
    (local.get $n2)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n2)
        (local.get $n2)
        (i32.const 1)
        i32.sub
        (call $fact1)
        i32.mul)))
  (func
    $is-even2
    (export "is-even")
    (param $n3 i32)
    (result i32)
    (local.get $n3)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else (local.get $n3) (i32.const 1) i32.sub (call $is-odd3))))
  (func
    $is-odd3
    (export "is-odd")
    (param $n4 i32)
    (result i32)
    (local.get $n4)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 0))
      (else (local.get $n4) (i32.const 1) i32.sub (call $is-even2))))
  (func
    $test-fact4
    (export "test-fact")
    (result i32)
    (i32.const 5)
    (call $fact1)))
```

</details>

<details>
<summary>Compiled web assembly (with tailcalls)</summary>

<!-- $MDX file=examples/readme.tc.wat -->
```wat
(module
  (func
    $ackermann0
    (export "ackermann")
    (param $m0 i32)
    (param $n1 i32)
    (result i32)
    (local.get $m0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $n1) (i32.const 1) i32.add)
      (else
        (local.get $n1)
        (i32.const 0)
        i32.eq
        (if
          (result i32)
          (then
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (i32.const 1)
            (return_call $ackermann0))
          (else
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (local.get $m0)
            (local.get $n1)
            (i32.const 1)
            i32.sub
            (call $ackermann0)
            (return_call $ackermann0))))))
  (func
    $fact1
    (export "fact")
    (param $n2 i32)
    (result i32)
    (local.get $n2)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n2)
        (local.get $n2)
        (i32.const 1)
        i32.sub
        (call $fact1)
        i32.mul)))
  (func
    $is-even2
    (export "is-even")
    (param $n3 i32)
    (result i32)
    (local.get $n3)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else (local.get $n3) (i32.const 1) i32.sub (return_call $is-odd3))))
  (func
    $is-odd3
    (export "is-odd")
    (param $n4 i32)
    (result i32)
    (local.get $n4)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 0))
      (else (local.get $n4) (i32.const 1) i32.sub (return_call $is-even2))))
  (func
    $test-fact4
    (export "test-fact")
    (result i32)
    (i32.const 5)
    (return_call $fact1)))
```

</details>

## Compiler overview

After parsing, the surface language is elaborated to a core language, using a
similar approach to the [elaboration projects](../README.md#elaboration).
The resulting program is then translated to web assembly.

```text
      Surface.Program.t
            │
            │    Surface.Elab.check_program
            │
            ▼
      Core.Program.t
            │
            │    Hoist.translate_program
            │
            ▼
        Wat.module_
            │
            │    Wat.Emit.pp_module_
            │
            ▼
  Format.formatter -> unit
```

## Todo list

- [x] Compile Core to WASM
  - [ ] Apply optimisations with [wasm-opt](https://github.com/WebAssembly/binaryen)
  - [x] Validate WAT with [wabt](https://github.com/WebAssembly/wabt)
- [ ] Compile Core to ANF
  - [ ] Generate join points
- [ ] Compile ANF to LLVM
- [ ] Compile Core to JavaScript
- [ ] Test that each translation preserves the semantics

CLI Entrypoints:

- [ ] REPL
- [ ] Elab
- [ ] Elab -> Hoist
- [x] Elab -> Hoist -> Emit WAT
