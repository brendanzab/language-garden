(module
  (export "fact" (func $fact))
  (export "fact-acc" (func $fact-acc))
  (export "test-fact" (func $test-fact))
  (func
    $fact
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 1)
    (call $fact-acc))
  (func
    $fact-acc
    (param $n i32)
    (param $acc i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $acc))
      (else
        (local.get $n)
        (i32.const 1)
        i32.sub
        (local.get $n)
        (local.get $acc)
        i32.mul
        (call $fact-acc))))
  (func $test-fact (result i32) (i32.const 5) (call $fact)))