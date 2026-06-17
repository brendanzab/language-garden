(module
  (export "test" (func $test))
  (export "text-shadow" (func $text-shadow))
  (func
    $test
    (param $b i32)
    (result i32)
    (local $x i32)
    (local $y i32)
    (i32.const 42)
    (local.set $x)
    (local.get $b)
    (if
      (result i32)
      (then (local.get $x) (i32.const 1) i32.add)
      (else (local.get $x) (i32.const 1) i32.sub))
    (local.set $y)
    (local.get $y)
    (local.get $x)
    i32.add)
  (func
    $text-shadow
    (param $x i32)
    (result i32)
    (local $x_1 i32)
    (local $x_2 i32)
    (local.get $x)
    (i32.const 1)
    i32.add
    (local.set $x_1)
    (local.get $x_1)
    (i32.const 3)
    i32.mul
    (local.set $x_2)
    (local.get $x_2)))