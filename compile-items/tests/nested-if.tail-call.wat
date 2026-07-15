(module
  (export "nested-if" (func $nested-if))
  (func
    $nested-if
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then
        (local.get $y)
        (i32.const 0)
        i32.eq
        (if
          (result i32)
          (then (i32.const 1000000000))
          (else (local.get $y) (i32.const 100) i32.add))
        (i32.const 3)
        i32.add)
      (else (local.get $x) (i32.const 100) i32.add))
    (i32.const 4)
    i32.add))