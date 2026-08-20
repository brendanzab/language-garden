# Compiling a first-order functional language with top-level items

- Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional) (+ top-level items, compilation) (- higher-order functions, currying)
- Based on [**scraps/compile_items_wasm.ml**](../scraps/compile_items_wasm.ml)

---

A demonstration of compiling a language with top-level, mutually recursive
definitions to Wasm and LLVM IR. This is a stepping stone towards compiling
higher-order, curried functional languages (like STLC or System-F), avoiding
the complexities of closure conversion, uncurrying, and memory allocation for
now.

## Example

<!-- $MDX file=examples/fact-tailrec.txt -->
```text
pub val test-fact : I32 :=
  fact(5);

pub fun fact(n : I32) : I32 :=
  fact-acc(n, 1);

fun fact-acc(n : I32, acc : I32) : I32 :=
  if n = 0 then acc else
    fact-acc(n - 1, n * acc);
```

<details>
<summary>Compiled Wasm</summary>

<!-- $MDX file=examples/fact-tailrec.wat -->
```wat
(module
  (export "fact" (func $fact))
  (export "test-fact" (func $test-fact))
  (func
    $fact
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 1)
    (call $fact-acc))
  (func
    $fact-acc
    (param $n i32)
    (param $acc i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $acc))
      (else
        (local.get $n)
        (i32.const 1)
        i32.sub
        (local.get $n)
        (local.get $acc)
        i32.mul
        (call $fact-acc))))
  (func $test-fact (result i32) (i32.const 5) (call $fact)))
```

</details>

<details>
<summary>Compiled Wasm (with tailcalls)</summary>

<!-- $MDX file=examples/fact-tailrec.tail-call.wat -->
```wat
(module
  (export "fact" (func $fact))
  (export "test-fact" (func $test-fact))
  (func
    $fact
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 1)
    (return_call $fact-acc))
  (func
    $fact-acc
    (param $n i32)
    (param $acc i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $acc))
      (else
        (local.get $n)
        (i32.const 1)
        i32.sub
        (local.get $n)
        (local.get $acc)
        i32.mul
        (return_call $fact-acc))))
  (func $test-fact (result i32) (i32.const 5) (return_call $fact)))
```

</details>

<details>
<summary>Compiled A-Normal Form</summary>

<!-- $MDX file=examples/fact-tailrec.anf -->
```text
pub fun fact(n : I32) : I32 := fact-acc(n, 1);

priv fun fact-acc(n : I32, acc : I32) : I32 :=
  let cond : Bool := #i32-eq(n, 0);
  join if_end (result : I32) := result;
  if cond then
    jump if_end acc
  else
    let arg : I32 := #i32-sub(n, 1);
    let arg_1 : I32 := #i32-mul(n, acc);
    let result_1 : I32 := fact-acc(arg, arg_1);
    jump if_end result_1;

pub val test-fact : I32 := fact(5);
```

</details>

<details>
<summary>Compiled LLVM IR</summary>

<!-- $MDX file=examples/fact-tailrec.ll -->
```ll
define i32 @fact(i32 %n) {
entry:
  %result = call i32 @fact-acc(i32 %n, i32 1)
  ret i32 %result
}

define private i32 @fact-acc(i32 %n, i32 %acc) {
entry:
  %cond = icmp eq i32 %n, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  %arg = sub i32 %n, 1
  %arg_1 = mul i32 %n, %acc
  %false_result = call i32 @fact-acc(i32 %arg, i32 %arg_1)
  br label %if_end
if_end:
  %result = phi i32 [%acc, %if_true], [%false_result, %if_false]
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
The resulting program is then translated to Wasm or LLVM.

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
      │  Core_to_wasm       │  Core_to_anf      │
      │                     │                   │
      ▼                     ▼                   │
  Wasm.module_         Anf.Module.t             │  Core_to_llvm
                            │                   │
                            │  Anf_to_llvm      │
                            │                   │
                            └─────────┬─────────┘
                                      │
                                      ▼
                                Llvm.program
```

## Todo list

- [x] Compile Core to WASM
  - [ ] Apply optimisations with [wasm-opt](https://github.com/WebAssembly/binaryen)
  - [x] Validate WAT with [wabt](https://github.com/WebAssembly/wabt)
- [x] Compile Core to ANF
  - [x] Generate join points
- [x] Compile Core to LLVM
- [x] Compile ANF to LLVM
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
- [x] `compile-anf-llvm`: Surface -> Core -> ANF -> LLVM
- [x] `compile-llvm`: Surface -> Core -> LLVM
