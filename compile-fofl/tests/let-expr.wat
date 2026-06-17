(module
  (export "test" (func $test0))
  (func
    $test0
    (param $b0 i32)
    (result i32)
    (local $x0 i32)
    (local $y1 i32)
    (i32.const 42)
    (local.set $x0)
    (local.get $b0)
    (if
      (result i32)
      (then (local.get $x0) (i32.const 1) i32.add)
      (else (local.get $x0) (i32.const 1) i32.sub))
    (local.set $y1)
    (local.get $y1)
    (local.get $x0)
    i32.add))