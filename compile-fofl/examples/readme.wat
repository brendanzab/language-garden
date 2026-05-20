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
            (call $ackermann0))
          (else
            (local.get $m0)
            (i32.const 1)
            i32.sub
            (local.get $m0)
            (local.get $n1)
            (i32.const 1)
            i32.sub
            (call $ackermann0)
            (call $ackermann0))))))
  (func
    $fact1
    (export "fact")
    (param $n2 i32)
    (result i32)
    (local.get $n2)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else
        (local.get $n2)
        (local.get $n2)
        (i32.const 1)
        i32.sub
        (call $fact1)
        i32.mul)))
  (func
    $is-even2
    (export "is-even")
    (param $n3 i32)
    (result i32)
    (local.get $n3)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else (local.get $n3) (i32.const 1) i32.sub (call $is-odd3))))
  (func
    $is-odd3
    (export "is-odd")
    (param $n4 i32)
    (result i32)
    (local.get $n4)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 0))
      (else (local.get $n4) (i32.const 1) i32.sub (call $is-even2))))
  (func
    $test-fact4
    (export "test-fact")
    (result i32)
    (i32.const 5)
    (call $fact1)))