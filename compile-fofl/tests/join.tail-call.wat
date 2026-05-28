(module
  (func
    $add_one0
    (export "add_one")
    (param $x0 i32)
    (result i32)
    (local.get $x0)
    (i32.const 1)
    i32.add)
  (func
    $test1
    (export "test")
    (param $x1 i32)
    (param $y2 i32)
    (result i32)
    (local.get $x1)
    (i32.const 0)
    i32.eq
    (if (result i32) (then (local.get $y2)) (else (local.get $x1)))
    (i32.const 1)
    i32.add)
  (func
    $test-tailcall2
    (export "test-tailcall")
    (param $x3 i32)
    (param $y4 i32)
    (result i32)
    (local.get $x3)
    (i32.const 0)
    i32.eq
    (if (result i32) (then (local.get $x3)) (else (local.get $y4)))
    (return_call $add_one0)))