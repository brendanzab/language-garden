(module
  (func
    $fact
    (export "fact")
    (param $n0 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n0) (i32.const 0))
      (then (return (i32.const 1)))
      (else
        (return
          (i32.mul
            (local.get $n0)
            (call $fact (i32.sub (local.get $n0) (i32.const 1))))))))
  (func
    $test-fact
    (export "test-fact")
    (result i32)
    (return_call $fact (i32.const 5))))