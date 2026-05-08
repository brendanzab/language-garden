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
    $ackermann
    (export "ackermann")
    (param $m0 i32)
    (param $n1 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $m0) (i32.const 0))
      (then (return (i32.add (local.get $n1) (i32.const 1))))
      (else
        (if
          (result i32)
          (i32.eq (local.get $n1) (i32.const 0))
          (then
            (return
              (call
                $ackermann
                (i32.sub (local.get $m0) (i32.const 1))
                (i32.const 1))))
          (else
            (return
              (call
                $ackermann
                (i32.sub (local.get $m0) (i32.const 1))
                (call
                  $ackermann
                  (local.get $m0)
                  (i32.sub (local.get $n1) (i32.const 1))))))))))
  (func
    $fact
    (export "fact")
    (param $n2 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n2) (i32.const 0))
      (then (return (i32.const 1)))
      (else
        (return
          (i32.mul
            (local.get $n2)
            (call $fact (i32.sub (local.get $n2) (i32.const 1))))))))
  (func
    $is-even
    (export "is-even")
    (param $n3 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n3) (i32.const 0))
      (then (return (i32.const 1)))
      (else (return (call $is-odd (i32.sub (local.get $n3) (i32.const 1)))))))
  (func
    $is-odd
    (export "is-odd")
    (param $n4 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n4) (i32.const 0))
      (then (return (i32.const 0)))
      (else (return (call $is-even (i32.sub (local.get $n4) (i32.const 1)))))))
  (func
    $test-fact
    (export "test-fact")
    (result i32)
    (return (call $fact (i32.const 5)))))
```

</details>

<details>
<summary>Compiled web assembly (with tailcalls)</summary>

<!-- $MDX file=examples/readme.tc.wat -->
```wat
(module
  (func
    $ackermann
    (export "ackermann")
    (param $m0 i32)
    (param $n1 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $m0) (i32.const 0))
      (then (return (i32.add (local.get $n1) (i32.const 1))))
      (else
        (if
          (result i32)
          (i32.eq (local.get $n1) (i32.const 0))
          (then
            (return_call
              $ackermann
              (i32.sub (local.get $m0) (i32.const 1))
              (i32.const 1)))
          (else
            (return_call
              $ackermann
              (i32.sub (local.get $m0) (i32.const 1))
              (call
                $ackermann
                (local.get $m0)
                (i32.sub (local.get $n1) (i32.const 1)))))))))
  (func
    $fact
    (export "fact")
    (param $n2 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n2) (i32.const 0))
      (then (return (i32.const 1)))
      (else
        (return
          (i32.mul
            (local.get $n2)
            (call $fact (i32.sub (local.get $n2) (i32.const 1))))))))
  (func
    $is-even
    (export "is-even")
    (param $n3 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n3) (i32.const 0))
      (then (return (i32.const 1)))
      (else (return_call $is-odd (i32.sub (local.get $n3) (i32.const 1))))))
  (func
    $is-odd
    (export "is-odd")
    (param $n4 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n4) (i32.const 0))
      (then (return (i32.const 0)))
      (else (return_call $is-even (i32.sub (local.get $n4) (i32.const 1))))))
  (func
    $test-fact
    (export "test-fact")
    (result i32)
    (return_call $fact (i32.const 5))))
```

</details>

## Compiler overview

After parsing, the surface language is elaborated to a core language, using a
similar approach to the [elaboration projects](../README.md#elaboration).
Following this, let bindings and conditionals are hoisted to the top of
expressions. The resulting program is then emitted as WAT.

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
    Hoisted.Program.t
            │
            │    Emit_wat.pp_program
            │
            ▼
  Format.formatter -> unit
```

## Todo list

- [x] Hoist let expressions and conditionals
  - [ ] Generate join points
- [x] Compile to WASM
  - [ ] Emit code for join points using blocks
  - [ ] Apply optimisations with [wasm-opt](https://github.com/WebAssembly/binaryen)
  - [ ] Validate WAT against an existing WASM toolchain
- [ ] Compile to LLVM
- [ ] Test that each translation preserves the semantics

CLI Entrypoints:

- [ ] REPL
- [ ] Elab
- [ ] Elab -> Hoist
- [x] Elab -> Hoist -> Emit WAT
