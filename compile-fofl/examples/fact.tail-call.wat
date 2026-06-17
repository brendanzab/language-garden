(module
  (export "fact" (func $fact0))
  (export "test-fact" (func $test-fact1))
  (func
    $fact0
    (param $n0 i32)
    (result i32)
    (local.get $n0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n0)
        (local.get $n0)
        (i32.const 1)
        i32.sub
        (call $fact0)
        i32.mul)))
  (func $test-fact1 (result i32) (i32.const 5) (return_call $fact0)))