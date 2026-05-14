(module
  (func
    $add_one
    (export "add_one")
    (param $x0 i32)
    (result i32)
    (return (i32.add (local.get $x0) (i32.const 1))))
  (func
    $test
    (export "test")
    (param $x1 i32)
    (param $y2 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $x1) (i32.const 0))
      (then (return (i32.add (local.get $y2) (i32.const 1))))
      (else (return (i32.add (local.get $x1) (i32.const 1))))))
  (func
    $test-tailcall
    (export "test-tailcall")
    (param $x3 i32)
    (param $y4 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $x3) (i32.const 0))
      (then (return_call $add_one (local.get $x3)))
      (else (return_call $add_one (local.get $y4))))))