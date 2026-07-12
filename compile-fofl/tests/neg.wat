(module
  (export "neg" (func $neg))
  (export "neg-expr" (func $neg-expr))
  (export "neg-neg" (func $neg-neg))
  (export "neg-prim" (func $neg-prim))
  (func
    $neg
    (param $x i32)
    (result i32)
    (i32.const 0)
    (local.get $x)
    i32.sub)
  (func
    $neg-expr
    (param $x i32)
    (param $y i32)
    (result i32)
    (i32.const 0)
    (local.get $x)
    (local.get $y)
    i32.sub
    i32.sub)
  (func
    $neg-neg
    (param $x i32)
    (result i32)
    (i32.const 0)
    (i32.const 0)
    (local.get $x)
    i32.sub
    i32.sub)
  (func
    $neg-prim
    (param $x i32)
    (result i32)
    (i32.const 0)
    (local.get $x)
    i32.sub))