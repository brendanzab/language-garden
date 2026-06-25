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
```text
val test-fact : I32 :=
  fact(5);

fun fact(n : I32) : I32 :=
  if n = 0 then 1 else n * fact(n - 1);
```

<details>
<summary>Compiled Web Assembly</summary>

<!-- $MDX file=examples/fact.wat -->
```wat
(module
  (export "fact" (func $fact))
  (export "test-fact" (func $test-fact))
  (func
    $fact
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n)
        (local.get $n)
        (i32.const 1)
        i32.sub
        (call $fact)
        i32.mul)))
  (func $test-fact (result i32) (i32.const 5) (call $fact)))
```

</details>

<details>
<summary>Compiled web assembly (with tailcalls)</summary>

<!-- $MDX file=examples/fact.tail-call.wat -->
```wat
(module
  (export "fact" (func $fact))
  (export "test-fact" (func $test-fact))
  (func
    $fact
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n)
        (local.get $n)
        (i32.const 1)
        i32.sub
        (call $fact)
        i32.mul)))
  (func $test-fact (result i32) (i32.const 5) (return_call $fact)))
```

</details>

<details>
<summary>Compiled A-Normal Form</summary>

<!-- $MDX file=examples/fact.anf -->
```text
fun fact(n : I32) : I32 :=
  let cond := #i32-eq(n, 0);
  join branch (result : I32) := result;
  if cond then
    jump branch 1
  else
    let arg := #i32-sub(n, 1);
    let arg_1 := fact(arg);
    let result_1 := #i32-mul(n, arg_1);
    jump branch result_1;

val test-fact : I32 := fact(5);
```

</details>

<details>
<summary>Compiled LLVM IR</summary>

<!-- $MDX file=examples/fact.ll -->
```ll
define i32 @fact(i32 %n) {
entry:
  %cond = icmp eq i32 %n, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_true_end
if_false:
  %arg = sub i32 %n, 1
  %arg_1 = call i32 @fact(i32 %arg)
  %false_result = mul i32 %n, %arg_1
  br label %if_false_end
if_true_end:
  br label %if_end
if_false_end:
  br label %if_end
if_end:
  %result = phi i32 [1, %if_true_end], [%false_result, %if_false_end]
  ret i32 %result
}

define i32 @test-fact() {
entry:
  %result = call i32 @fact(i32 5)
  ret i32 %result
}
```

</details>

## Compiler overview

After parsing, the surface language is elaborated to a core language, using a
similar approach to the [elaboration projects](../README.md#elaboration).
The resulting program is then translated to web assembly.

```text
               Surface.Module.t
                     │
                     │  Surface.Elab
                     │
                     ▼
               Core.Module.t
                     │
      ┌──────────────┴──────┬───────────────────┐
      │                     │                   │
      │  Core_to_wat        │  Core_to_anf      │  Core_to_llvm
      │                     │                   │
      ▼                     ▼                   ▼
  Wat.module_          Anf.Module.t        Llvm.program
```

## Todo list

- [x] Compile Core to WASM
  - [ ] Apply optimisations with [wasm-opt](https://github.com/WebAssembly/binaryen)
  - [x] Validate WAT with [wabt](https://github.com/WebAssembly/wabt)
- [x] Compile Core to ANF
  - [x] Generate join points
- [x] Compile Core to LLVM
- [ ] Compile Core to JavaScript
- [ ] Test that each translation preserves the semantics

CLI Entrypoints:

- [ ] `repl`
- [ ] `elab`: Surface -> Core
- [ ] `doc`: Surface -> Doc
- [ ] `eval`: Surface -> Core -> Value
- [ ] `eval-anf`: Surface -> Core -> ANF -> Value
- [x] `compile-wat`: Surface -> Core -> WAT
- [x] `compile-anf`: Surface -> Core -> ANF
- [x] `compile-llvm`: Surface -> Core -> LLVM
