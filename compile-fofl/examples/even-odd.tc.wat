(module
  (func
    $is-even
    (export "is-even")
    (param $n0 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n0) (i32.const 0))
      (then (return (i32.const 1)))
      (else (return_call $is-odd (i32.sub (local.get $n0) (i32.const 1))))))
  (func
    $is-odd
    (export "is-odd")
    (param $n1 i32)
    (result i32)
    (if
      (result i32)
      (i32.eq (local.get $n1) (i32.const 0))
      (then (return (i32.const 0)))
      (else (return_call $is-even (i32.sub (local.get $n1) (i32.const 1)))))))