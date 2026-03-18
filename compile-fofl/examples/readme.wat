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
                  (i32.sub (local.get $n1) (i32.const 1))))))))))
  (func
    $fact
    (export "fact")
    (param $n2 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n2) (i32.const 0))
      (then (return (i32.const 1)))
      (else
        (return
          (i32.mul
            (local.get $n2)
            (call $fact (i32.sub (local.get $n2) (i32.const 1))))))))
  (func
    $is-even
    (export "is-even")
    (param $n3 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n3) (i32.const 0))
      (then (return (i32.const 1)))
      (else (return (call $is-odd (i32.sub (local.get $n3) (i32.const 1)))))))
  (func
    $is-odd
    (export "is-odd")
    (param $n4 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n4) (i32.const 0))
      (then (return (i32.const 0)))
      (else (return (call $is-even (i32.sub (local.get $n4) (i32.const 1)))))))
  (func
    $test-fact
    (export "test-fact")
    (result i32)
    (return (call $fact (i32.const 5)))))