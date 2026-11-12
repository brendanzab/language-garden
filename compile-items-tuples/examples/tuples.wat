(module
  (type $Tuple (struct (field i32 i32)))
  (type $Tuple_1 (struct (field (ref $Tuple) i32)))
  (type $Tuple_2 (struct (field i32 i32 (ref $Tuple_1) i32)))
  (type $Tuple_3 (struct (field i32 i32 i32)))
  (type $Tuple_4 (struct (field i32)))
  (type $Tuple_5 (struct ))
  (func
    $fst
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 0))
  (func
    $lookup-nested
    (result i32)
    (call $nested)
    (struct.get $Tuple_2 2)
    (struct.get $Tuple_1 0)
    (struct.get $Tuple 1))
  (func
    $mul
    (param $v1 (ref $Tuple_3))
    (param $v2 (ref $Tuple_3))
    (result (ref $Tuple_3))
    (local.get $v1)
    (struct.get $Tuple_3 0)
    (local.get $v2)
    (struct.get $Tuple_3 0)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_3 1)
    (local.get $v2)
    (struct.get $Tuple_3 1)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_3 2)
    (local.get $v2)
    (struct.get $Tuple_3 2)
    i32.mul
    (struct.new $Tuple_3))
  (func
    $mul-scalar
    (param $v (ref $Tuple_3))
    (param $s i32)
    (result (ref $Tuple_3))
    (local.get $v)
    (struct.get $Tuple_3 0)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_3 1)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_3 2)
    (local.get $s)
    i32.mul
    (struct.new $Tuple_3))
  (func
    $nested
    (result (ref $Tuple_2))
    (i32.const 33)
    (i32.const 4)
    (call $pair)
    (i32.const 1)
    (struct.new $Tuple_1)
    (i32.const 0)
    (struct.new $Tuple_2))
  (func
    $pair
    (result (ref $Tuple))
    (i32.const 33)
    (i32.const 0)
    (struct.new $Tuple))
  (func
    $singleton
    (result (ref $Tuple_4))
    (i32.const 33)
    (struct.new $Tuple_4))
  (func
    $snd
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 1))
  (func $unit (result (ref $Tuple_5)) (struct.new $Tuple_5)))