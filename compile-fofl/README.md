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

<!-- $MDX file=examples/fact.txt -->
```
val test-fact : I32 :=
  fact(5);

fun fact(n : I32) : I32 :=
  if n = 0 then 1 else n * fact(n - 1);
```

<details>
<summary>Compiled web assembly</summary>

<!-- $MDX file=examples/fact.wat -->
```wat
(module
  (func
    $fact0
    (export "fact")
    (param $n0 i32)
    (result i32)
    (local.get $n0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n0)
        (local.get $n0)
        (i32.const 1)
        i32.sub
        (call $fact0)
        i32.mul)))
  (func
    $test-fact1
    (export "test-fact")
    (result i32)
    (i32.const 5)
    (call $fact0)))
```

</details>

<details>
<summary>Compiled web assembly (with tailcalls)</summary>

<!-- $MDX file=examples/fact.tc.wat -->
```wat
(module
  (func
    $fact0
    (export "fact")
    (param $n0 i32)
    (result i32)
    (local.get $n0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n0)
        (local.get $n0)
        (i32.const 1)
        i32.sub
        (call $fact0)
        i32.mul)))
  (func
    $test-fact1
    (export "test-fact")
    (result i32)
    (i32.const 5)
    (return_call $fact0)))
```

</details>

## Compiler overview

After parsing, the surface language is elaborated to a core language, using a
similar approach to the [elaboration projects](../README.md#elaboration).
The resulting program is then translated to web assembly.

```text
      Surface.Module.t
            │
            │    Surface.Elab.check_module
            │
            ▼
      Core.Module.t
            │
            │    Core_to_wat.translate_module
            │
            ▼
        Wat.module_
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

- [ ] `repl`: REPL
- [ ] `elab`: Elab
- [ ] `doc`: Elab -> Doc
- [ ] `eval`: Elab -> Eval
- [x] `compile-wat`: Elab -> WAT
