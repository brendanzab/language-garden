(module
  (export "is-even" (func $is-even0))
  (export "is-odd" (func $is-odd1))
  (func
    $is-even0
    (param $n0 i32)
    (result i32)
    (local.get $n0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else (local.get $n0) (i32.const 1) i32.sub (return_call $is-odd1))))
  (func
    $is-odd1
    (param $n0 i32)
    (result i32)
    (local.get $n0)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 0))
      (else (local.get $n0) (i32.const 1) i32.sub (return_call $is-even0)))))