# Compiling a language with top-level items and indirect calls

- Extends [**compile-items**](../compile-items) (+ function references, indirect calls)

---

The [compile-items](../compile-items) project requires all functions to be fully
applied. This project lifts that restriction by adding support for function
types, passing references to functions as arguments, and storing them as local
or top-level definitions.

This is relatively straightforward to support in LLVM IR, but the Wasm back-end
requires a little more effort. This involves:

- registering type definitions for the function types used in the program
- registering each function reference in a global table
- emitting `call` and `call_ref` instructions as needed

Note that this project does _not_ support anonymous functions. This would
require additional work to implement, namely closure conversion.

## Example

<!-- $MDX file=examples/funrefs.txt -->
```text
fun choose(b : Bool, f : fun (I32) -> I32, g : fun (I32) -> I32) : fun (I32) -> I32 :=
  if b then f else g;

fun incr(i : I32) : I32 := i + 1;
fun decr(i : I32) : I32 := i - 1;

val test-true : I32 := choose(true, incr, decr)(42);
val test-false : I32 := choose(false, incr, decr)(42);

val partial-app : fun (I32) -> I32 :=
  choose(true, incr, decr);

val test-partial-app : I32 :=
  partial-app(42);
```

<details>
<summary>Compiled Wasm</summary>

<!-- $MDX file=examples/funrefs.wat -->
```wat
(module
  (export "choose" (func $choose))
  (export "decr" (func $decr))
  (export "incr" (func $incr))
  (export "partial-app" (func $partial-app))
  (export "test-false" (func $test-false))
  (export "test-partial-app" (func $test-partial-app))
  (export "test-true" (func $test-true))
  (type $funty (func (param i32) (result i32)))
  (elem declare func $incr $decr)
  (func
    $choose
    (param $b i32)
    (param $f (ref $funty))
    (param $g (ref $funty))
    (result (ref $funty))
    (local.get $b)
    (if (result (ref $funty)) (then (local.get $f)) (else (local.get $g))))
  (func
    $decr
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.sub)
  (func
    $incr
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.add)
  (func
    $partial-app
    (result (ref $funty))
    (i32.const 1)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose))
  (func
    $test-false
    (result i32)
    (i32.const 42)
    (i32.const 0)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose)
    (call_ref $funty))
  (func
    $test-partial-app
    (result i32)
    (i32.const 42)
    (call $partial-app)
    (call_ref $funty))
  (func
    $test-true
    (result i32)
    (i32.const 42)
    (i32.const 1)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose)
    (call_ref $funty)))
```

</details>

<details>
<summary>Compiled Wasm (with tailcalls)</summary>

<!-- $MDX file=examples/funrefs.tail-call.wat -->
```wat
(module
  (export "choose" (func $choose))
  (export "decr" (func $decr))
  (export "incr" (func $incr))
  (export "partial-app" (func $partial-app))
  (export "test-false" (func $test-false))
  (export "test-partial-app" (func $test-partial-app))
  (export "test-true" (func $test-true))
  (type $funty (func (param i32) (result i32)))
  (elem declare func $incr $decr)
  (func
    $choose
    (param $b i32)
    (param $f (ref $funty))
    (param $g (ref $funty))
    (result (ref $funty))
    (local.get $b)
    (if (result (ref $funty)) (then (local.get $f)) (else (local.get $g))))
  (func
    $decr
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.sub)
  (func
    $incr
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.add)
  (func
    $partial-app
    (result (ref $funty))
    (i32.const 1)
    (ref.func $incr)
    (ref.func $decr)
    (return_call $choose))
  (func
    $test-false
    (result i32)
    (i32.const 42)
    (i32.const 0)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose)
    (return_call_ref $funty))
  (func
    $test-partial-app
    (result i32)
    (i32.const 42)
    (call $partial-app)
    (return_call_ref $funty))
  (func
    $test-true
    (result i32)
    (i32.const 42)
    (i32.const 1)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose)
    (return_call_ref $funty)))
```

</details>

<details>
<summary>Compiled A-Normal Form</summary>

<!-- $MDX file=examples/funrefs.anf -->
```text
fun choose(b : Bool, f : fun (I32) -> I32, g : fun (I32) -> I32) :
fun (I32) -> I32 :=
  join if_end (result : fun (I32) -> I32) := result;
  if b then jump if_end f else jump if_end g;

fun decr(i : I32) : I32 := #i32-sub(i, 1);

fun incr(i : I32) : I32 := #i32-add(i, 1);

val partial-app : fun (I32) -> I32 := choose(true, incr, decr);

val test-false : I32 := let fun := choose(false, incr, decr);
                        fun(42);

val test-partial-app : I32 := partial-app(42);

val test-true : I32 := let fun := choose(true, incr, decr);
                       fun(42);
```

</details>

<details>
<summary>Compiled LLVM IR</summary>

<!-- $MDX file=examples/funrefs.ll -->
```ll
define i32(i32)* @choose(i1 %b, i32(i32)* %f, i32(i32)* %g) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  br label %if_end
if_end:
  %result = phi i32(i32)* [%f, %if_true], [%g, %if_false]
  ret i32(i32)* %result
}

define i32 @decr(i32 %i) {
entry:
  %result = sub i32 %i, 1
  ret i32 %result
}

define i32 @incr(i32 %i) {
entry:
  %result = add i32 %i, 1
  ret i32 %result
}

define i32(i32)* @partial-app() {
entry:
  %result = call i32(i32)* @choose(i1 true, i32(i32)* @incr, i32(i32)* @decr)
  ret i32(i32)* %result
}

define i32 @test-false() {
entry:
  %fun = call i32(i32)* @choose(i1 false, i32(i32)* @incr, i32(i32)* @decr)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-partial-app() {
entry:
  %fun = call i32(i32)* @partial-app()
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-true() {
entry:
  %fun = call i32(i32)* @choose(i1 true, i32(i32)* @incr, i32(i32)* @decr)
  %result = call i32 %fun(i32 42)
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
      │  Core_to_wasm       │  Core_to_anf      │  Core_to_llvm
      │                     │                   │
      ▼                     ▼                   ▼
  Wasm.module_         Anf.Module.t        Llvm.program
```
