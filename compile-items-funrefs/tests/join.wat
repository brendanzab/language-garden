(module
  (export "add_one" (func $add_one))
  (export "test" (func $test))
  (export "test-tailcall" (func $test-tailcall))
  (func
    $add_one
    (param $x i32)
    (result i32)
    (local.get $x)
    (i32.const 1)
    i32.add)
  (func
    $test
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (i32.const 0)
    i32.eq
    (if (result i32) (then (local.get $y)) (else (local.get $x)))
    (i32.const 1)
    i32.add)
  (func
    $test-tailcall
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (i32.const 0)
    i32.eq
    (if (result i32) (then (local.get $x)) (else (local.get $y)))
    (call $add_one)))