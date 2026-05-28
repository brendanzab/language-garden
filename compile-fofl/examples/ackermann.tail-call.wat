(module
  (func
    $ackermann0
    (export "ackermann")
    (param $m0 i32)
    (param $n1 i32)
    (result i32)
    (local.get $m0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $n1) (i32.const 1) i32.add)
      (else
        (local.get $n1)
        (i32.const 0)
        i32.eq
        (if
          (result i32)
          (then
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (i32.const 1)
            (return_call $ackermann0))
          (else
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (local.get $m0)
            (local.get $n1)
            (i32.const 1)
            i32.sub
            (call $ackermann0)
            (return_call $ackermann0)))))))