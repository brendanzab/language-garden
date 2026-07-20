(module
  (export "is-even" (func $is-even))
  (export "is-odd" (func $is-odd))
  (func
    $is-even
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 1))
      (else (local.get $n) (i32.const 1) i32.sub (return_call $is-odd))))
  (func
    $is-odd
    (param $n i32)
    (result i32)
    (local.get $n)
    (i32.const 0)
    i32.eq
    (if
      (result i32)
      (then (i32.const 0))
      (else (local.get $n) (i32.const 1) i32.sub (return_call $is-even)))))