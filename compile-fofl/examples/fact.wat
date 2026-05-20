(module
  (func
    $fact0
    (export "fact")
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
  (func
    $test-fact1
    (export "test-fact")
    (result i32)
    (i32.const 5)
    (call $fact0)))