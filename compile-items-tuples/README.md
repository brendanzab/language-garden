# Compiling a language with top-level items and indirect calls

- Extends [**compile-items-funrefs**](../compile-items-funrefs) (+ tuples)

---

This project extends the [compile-items-funrefs](../compile-items-funrefs)
project with support for tuples.

## Example

<!-- $MDX file=examples/tuples.txt -->
```text
val unit : () := ();
val singleton : (I32,) := (33,);
val pair : (I32, Bool) := (33, false);

val nested : (I32, I32, ((I32, Bool), Bool), Bool) :=
  (33, 4, (pair, true), false);

val lookup-nested : Bool :=
  nested.2.0.1;

fun fst(p : (I32, I32)) : I32 := p.0;
fun snd(p : (I32, I32)) : I32 := p.1;

fun mul-scalar(v : (I32, I32, I32), s : I32) : (I32, I32, I32) :=
  (v.0 * s, v.1 * s, v.2 * s);

fun mul(v1 : (I32, I32, I32), v2 : (I32, I32, I32)) : (I32, I32, I32) :=
  (v1.0 * v2.0, v1.1 * v2.1, v1.2 * v2.2);
```

<details>
<summary>Compiled Wasm</summary>

<!-- $MDX file=examples/tuples.wat -->
```wat
(module
  (type $Tuple (struct (field i32 i32)))
  (type $Tuple_1 (struct (field (ref $Tuple) i32)))
  (type $Tuple_2 (struct (field i32 i32 (ref $Tuple_1) i32)))
  (type $Tuple_3 (struct (field i32 i32 i32)))
  (type $Tuple_4 (struct (field i32)))
  (type $Tuple_5 (struct ))
  (func
    $fst
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 0))
  (func
    $lookup-nested
    (result i32)
    (call $nested)
    (struct.get $Tuple_2 2)
    (struct.get $Tuple_1 0)
    (struct.get $Tuple 1))
  (func
    $mul
    (param $v1 (ref $Tuple_3))
    (param $v2 (ref $Tuple_3))
    (result (ref $Tuple_3))
    (local.get $v1)
    (struct.get $Tuple_3 0)
    (local.get $v2)
    (struct.get $Tuple_3 0)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_3 1)
    (local.get $v2)
    (struct.get $Tuple_3 1)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_3 2)
    (local.get $v2)
    (struct.get $Tuple_3 2)
    i32.mul
    (struct.new $Tuple_3))
  (func
    $mul-scalar
    (param $v (ref $Tuple_3))
    (param $s i32)
    (result (ref $Tuple_3))
    (local.get $v)
    (struct.get $Tuple_3 0)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_3 1)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_3 2)
    (local.get $s)
    i32.mul
    (struct.new $Tuple_3))
  (func
    $nested
    (result (ref $Tuple_2))
    (i32.const 33)
    (i32.const 4)
    (call $pair)
    (i32.const 1)
    (struct.new $Tuple_1)
    (i32.const 0)
    (struct.new $Tuple_2))
  (func
    $pair
    (result (ref $Tuple))
    (i32.const 33)
    (i32.const 0)
    (struct.new $Tuple))
  (func
    $singleton
    (result (ref $Tuple_4))
    (i32.const 33)
    (struct.new $Tuple_4))
  (func
    $snd
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 1))
  (func $unit (result (ref $Tuple_5)) (struct.new $Tuple_5)))
```

</details>

<details>
<summary>Compiled Wasm (with tailcalls)</summary>

<!-- $MDX file=examples/tuples.tail-call.wat -->
```wat
(module
  (type $Tuple (struct (field i32 i32)))
  (type $Tuple_1 (struct (field (ref $Tuple) i32)))
  (type $Tuple_2 (struct (field i32 i32 (ref $Tuple_1) i32)))
  (type $Tuple_3 (struct (field i32 i32 i32)))
  (type $Tuple_4 (struct (field i32)))
  (type $Tuple_5 (struct ))
  (func
    $fst
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 0))
  (func
    $lookup-nested
    (result i32)
    (call $nested)
    (struct.get $Tuple_2 2)
    (struct.get $Tuple_1 0)
    (struct.get $Tuple 1))
  (func
    $mul
    (param $v1 (ref $Tuple_3))
    (param $v2 (ref $Tuple_3))
    (result (ref $Tuple_3))
    (local.get $v1)
    (struct.get $Tuple_3 0)
    (local.get $v2)
    (struct.get $Tuple_3 0)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_3 1)
    (local.get $v2)
    (struct.get $Tuple_3 1)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_3 2)
    (local.get $v2)
    (struct.get $Tuple_3 2)
    i32.mul
    (struct.new $Tuple_3))
  (func
    $mul-scalar
    (param $v (ref $Tuple_3))
    (param $s i32)
    (result (ref $Tuple_3))
    (local.get $v)
    (struct.get $Tuple_3 0)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_3 1)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_3 2)
    (local.get $s)
    i32.mul
    (struct.new $Tuple_3))
  (func
    $nested
    (result (ref $Tuple_2))
    (i32.const 33)
    (i32.const 4)
    (call $pair)
    (i32.const 1)
    (struct.new $Tuple_1)
    (i32.const 0)
    (struct.new $Tuple_2))
  (func
    $pair
    (result (ref $Tuple))
    (i32.const 33)
    (i32.const 0)
    (struct.new $Tuple))
  (func
    $singleton
    (result (ref $Tuple_4))
    (i32.const 33)
    (struct.new $Tuple_4))
  (func
    $snd
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 1))
  (func $unit (result (ref $Tuple_5)) (struct.new $Tuple_5)))
```

</details>

<details>
<summary>Compiled A-Normal Form</summary>

<!-- $MDX file=examples/tuples.anf -->
```text
priv fun fst(p : (I32, I32)) : I32 := p.0;

priv val lookup-nested : Bool :=
  let tuple : ((I32, Bool), Bool) := nested.2;
  let tuple_1 : (I32, Bool) := tuple.0;
  tuple_1.1;

priv fun mul(v1 : (I32, I32, I32), v2 : (I32, I32, I32)) : (I32, I32, I32) :=
  let arg : I32 := v1.0;
  let arg_1 : I32 := v2.0;
  let elem : I32 := #i32-mul(arg, arg_1);
  let arg_2 : I32 := v1.1;
  let arg_3 : I32 := v2.1;
  let elem_1 : I32 := #i32-mul(arg_2, arg_3);
  let arg_4 : I32 := v1.2;
  let arg_5 : I32 := v2.2;
  let elem_2 : I32 := #i32-mul(arg_4, arg_5);
  (elem, elem_1, elem_2);

priv fun mul-scalar(v : (I32, I32, I32), s : I32) : (I32, I32, I32) :=
  let arg : I32 := v.0;
  let elem : I32 := #i32-mul(arg, s);
  let arg_1 : I32 := v.1;
  let elem_1 : I32 := #i32-mul(arg_1, s);
  let arg_2 : I32 := v.2;
  let elem_2 : I32 := #i32-mul(arg_2, s);
  (elem, elem_1, elem_2);

priv val nested : (I32, I32, ((I32, Bool), Bool), Bool) :=
  (33, 4, (pair, true), false);

priv val pair : (I32, Bool) := (33, false);

priv val singleton : (I32) := (33,);

priv fun snd(p : (I32, I32)) : I32 := p.1;

priv val unit : () := ();
```

</details>

<details>
<summary>Compiled LLVM IR</summary>

<!-- $MDX file=examples/tuples.ll -->
```ll
declare ptr @malloc(i32)

define private i32 @fst(ptr %p) {
entry:
  %result.ptr = getelementptr i8, ptr %p, i32 0
  %result = load i32, ptr %result.ptr
  ret i32 %result
}

define private i1 @lookup-nested() {
entry:
  %tuple = call ptr @nested()
  %tuple.ptr = getelementptr i8, ptr %tuple, i32 12
  %tuple_1 = load ptr, ptr %tuple.ptr
  %tuple.ptr_1 = getelementptr i8, ptr %tuple_1, i32 0
  %tuple_2 = load ptr, ptr %tuple.ptr_1
  %result.ptr = getelementptr i8, ptr %tuple_2, i32 1
  %result = load i1, ptr %result.ptr
  ret i1 %result
}

define private ptr @mul(ptr %v1, ptr %v2) {
entry:
  %result = call ptr @malloc(i32 12)
  %arg.ptr = getelementptr i8, ptr %v1, i32 0
  %arg = load i32, ptr %arg.ptr
  %arg.ptr_1 = getelementptr i8, ptr %v2, i32 0
  %arg_1 = load i32, ptr %arg.ptr_1
  %elem = mul i32 %arg, %arg_1
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 %elem, ptr %elem.dst
  %arg.ptr_2 = getelementptr i8, ptr %v1, i32 4
  %arg_2 = load i32, ptr %arg.ptr_2
  %arg.ptr_3 = getelementptr i8, ptr %v2, i32 4
  %arg_3 = load i32, ptr %arg.ptr_3
  %elem_1 = mul i32 %arg_2, %arg_3
  %elem.dst_1 = getelementptr i8, ptr %result, i32 4
  store i32 %elem_1, ptr %elem.dst_1
  %arg.ptr_4 = getelementptr i8, ptr %v1, i32 8
  %arg_4 = load i32, ptr %arg.ptr_4
  %arg.ptr_5 = getelementptr i8, ptr %v2, i32 8
  %arg_5 = load i32, ptr %arg.ptr_5
  %elem_2 = mul i32 %arg_4, %arg_5
  %elem.dst_2 = getelementptr i8, ptr %result, i32 8
  store i32 %elem_2, ptr %elem.dst_2
  ret ptr %result
}

define private ptr @mul-scalar(ptr %v, i32 %s) {
entry:
  %result = call ptr @malloc(i32 12)
  %arg.ptr = getelementptr i8, ptr %v, i32 0
  %arg = load i32, ptr %arg.ptr
  %elem = mul i32 %arg, %s
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 %elem, ptr %elem.dst
  %arg.ptr_1 = getelementptr i8, ptr %v, i32 4
  %arg_1 = load i32, ptr %arg.ptr_1
  %elem_1 = mul i32 %arg_1, %s
  %elem.dst_1 = getelementptr i8, ptr %result, i32 4
  store i32 %elem_1, ptr %elem.dst_1
  %arg.ptr_2 = getelementptr i8, ptr %v, i32 8
  %arg_2 = load i32, ptr %arg.ptr_2
  %elem_2 = mul i32 %arg_2, %s
  %elem.dst_2 = getelementptr i8, ptr %result, i32 8
  store i32 %elem_2, ptr %elem.dst_2
  ret ptr %result
}

define private ptr @nested() {
entry:
  %result = call ptr @malloc(i32 17)
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 33, ptr %elem.dst
  %elem.dst_1 = getelementptr i8, ptr %result, i32 4
  store i32 4, ptr %elem.dst_1
  %elem = call ptr @malloc(i32 9)
  %elem_1 = call ptr @pair()
  %elem.dst_2 = getelementptr i8, ptr %elem, i32 0
  store ptr %elem_1, ptr %elem.dst_2
  %elem.dst_3 = getelementptr i8, ptr %elem, i32 1
  store i1 true, ptr %elem.dst_3
  %elem.dst_4 = getelementptr i8, ptr %result, i32 12
  store ptr %elem, ptr %elem.dst_4
  %elem.dst_5 = getelementptr i8, ptr %result, i32 13
  store i1 false, ptr %elem.dst_5
  ret ptr %result
}

define private ptr @pair() {
entry:
  %result = call ptr @malloc(i32 5)
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 33, ptr %elem.dst
  %elem.dst_1 = getelementptr i8, ptr %result, i32 1
  store i1 false, ptr %elem.dst_1
  ret ptr %result
}

define private ptr @singleton() {
entry:
  %result = call ptr @malloc(i32 4)
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 33, ptr %elem.dst
  ret ptr %result
}

define private i32 @snd(ptr %p) {
entry:
  %result.ptr = getelementptr i8, ptr %p, i32 4
  %result = load i32, ptr %result.ptr
  ret i32 %result
}

define private ptr @unit() {
entry:
  %result = call ptr @malloc(i32 0)
  ret ptr %result
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
