(module
  (func $square (param $x i32) (result i32)
    (i32.mul (local.get $x) (local.get $x))
  )

  (func $assert (param $b i32)
    (if (local.get $b) (then) (else unreachable))
  )

  (func $main (export "main")
    (call $assert (i32.eq (call $square (i32.const 2)) (i32.const 4)))
    (call $assert (i32.eq (call $square (i32.const 2)) (i32.const 5)))
))
