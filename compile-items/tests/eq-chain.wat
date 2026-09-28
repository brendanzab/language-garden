(module
  (func
    $eq-chain
    (param $x i32)
    (result i32)
    (local.get $x)
    (i32.const 1)
    i32.eq
    (i32.const 0)
    i32.eq))