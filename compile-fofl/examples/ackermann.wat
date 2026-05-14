(module
  (func
    $ackermann
    (export "ackermann")
    (param $m0 i32)
    (param $n1 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $m0) (i32.const 0))
      (then (return (i32.add (local.get $n1) (i32.const 1))))
      (else
        (if
          (result i32)
          (i32.eq (local.get $n1) (i32.const 0))
          (then
            (return
              (call
                $ackermann
                (i32.sub (local.get $m0) (i32.const 1))
                (i32.const 1))))
          (else
            (return
              (call
                $ackermann
                (i32.sub (local.get $m0) (i32.const 1))
                (call
                  $ackermann
                  (local.get $m0)
                  (i32.sub (local.get $n1) (i32.const 1)))))))))))