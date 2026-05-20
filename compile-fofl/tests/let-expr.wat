(module
  (func
    $test0
    (export "test")
    (param $b0 i32)
    (result i32)
    (local $x1 i32)
    (local $y2 i32)
    (i32.const 42)
    (local.set $x1)
    (local.get $b0)
    (if
      (result i32)
      (then (local.get $x1) (i32.const 1) i32.add)
      (else (local.get $x1) (i32.const 1) i32.sub))
    (local.set $y2)
    (local.get $y2)
    (local.get $x1)
    i32.add))