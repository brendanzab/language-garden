(module
  (export "ackermann" (func $ackermann))
  (func
    $ackermann
    (param $m i32)
    (param $n i32)
    (result i32)
    (local.get $m)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (local.get $n) (i32.const 1) i32.add)
      (else
        (local.get $n)
        (i32.const 0)
        i32.eq
        (if
          (result i32)
          (then
            (local.get $m)
            (i32.const 1)
            i32.sub
            (i32.const 1)
            (call $ackermann))
          (else
            (local.get $m)
            (i32.const 1)
            i32.sub
            (local.get $m)
            (local.get $n)
            (i32.const 1)
            i32.sub
            (call $ackermann)
            (call $ackermann)))))))