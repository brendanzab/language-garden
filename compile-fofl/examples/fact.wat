(module
  (export "fact" (func $fact))
  (export "test-fact" (func $test-fact))
  (func
    $fact
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n)
        (local.get $n)
        (i32.const 1)
        i32.sub
        (call $fact)
        i32.mul)))
  (func $test-fact (result i32) (i32.const 5) (call $fact)))