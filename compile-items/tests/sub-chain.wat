(module
  (func
    $sub-chain
    (param $x i32)
    (result i32)
    (local.get $x)
    (i32.const 1)
    i32.sub
    (i32.const 2)
    i32.sub))