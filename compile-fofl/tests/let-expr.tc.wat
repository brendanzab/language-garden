(module
  (func
    $test
    (export "test")
    (param $b0 i32)
    (result i32)
    (local $x1 i32)
    (local $y2 i32)
    (local.set $x1 (i32.const 42))
    (if
      (result i32)
      (local.get $b0)
      (then
        (local.set $y2 (i32.add (local.get $x1) (i32.const 1)))
        (return (i32.add (local.get $y2) (local.get $x1))))
      (else
        (local.set $y2 (i32.sub (local.get $x1) (i32.const 1)))
        (return (i32.add (local.get $y2) (local.get $x1)))))))