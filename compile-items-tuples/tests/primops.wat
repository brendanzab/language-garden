(module
  (export "bool-eq-op" (func $bool-eq-op))
  (export "bool-eq-prim" (func $bool-eq-prim))
  (export "i32-add-op" (func $i32-add-op))
  (export "i32-add-prim" (func $i32-add-prim))
  (export "i32-eq-op" (func $i32-eq-op))
  (export "i32-eq-prim" (func $i32-eq-prim))
  (export "i32-mul-op" (func $i32-mul-op))
  (export "i32-mul-prim" (func $i32-mul-prim))
  (export "i32-sub-op" (func $i32-sub-op))
  (export "i32-sub-prim" (func $i32-sub-prim))
  (export "neg-op" (func $neg-op))
  (export "neg-prim" (func $neg-prim))
  (func
    $bool-eq-op
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.eq)
  (func
    $bool-eq-prim
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.eq)
  (func
    $i32-add-op
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.add)
  (func
    $i32-add-prim
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.add)
  (func
    $i32-eq-op
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.eq)
  (func
    $i32-eq-prim
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.eq)
  (func
    $i32-mul-op
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.mul)
  (func
    $i32-mul-prim
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.mul)
  (func
    $i32-sub-op
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.sub)
  (func
    $i32-sub-prim
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.sub)
  (func
    $neg-op
    (param $x i32)
    (result i32)
    (i32.const 0)
    (local.get $x)
    i32.sub)
  (func
    $neg-prim
    (param $x i32)
    (result i32)
    (i32.const 0)
    (local.get $x)
    i32.sub))