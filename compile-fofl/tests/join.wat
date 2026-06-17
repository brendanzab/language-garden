(module
  (export "add_one" (func $add_one0))
  (export "test" (func $test1))
  (export "test-tailcall" (func $test-tailcall2))
  (func
    $add_one0
    (param $x0 i32)
    (result i32)
    (local.get $x0)
    (i32.const 1)
    i32.add)
  (func
    $test1
    (param $x0 i32)
    (param $y1 i32)
    (result i32)
    (local.get $x0)
    (i32.const 0)
    i32.eq
    (if (result i32) (then (local.get $y1)) (else (local.get $x0)))
    (i32.const 1)
    i32.add)
  (func
    $test-tailcall2
    (param $x0 i32)
    (param $y1 i32)
    (result i32)
    (local.get $x0)
    (i32.const 0)
    i32.eq
    (if (result i32) (then (local.get $x0)) (else (local.get $y1)))
    (call $add_one0)))