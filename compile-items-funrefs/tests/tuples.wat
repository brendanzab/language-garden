(module
  (type $Tuple (struct (field i32 i32)))
  (type $Tuple_1 (struct (field i32 i32 i32)))
  (type $Tuple_2 (struct (field i32 (ref $Tuple))))
  (type $Tuple_3 (struct (field i32 (ref $Tuple_2) i32)))
  (type $Tuple_4 (struct (field i32)))
  (type $Tuple_5 (struct ))
  (func
    $fst
    (param $p (ref $Tuple))
    (result i32)
    (local.get $p)
    (struct.get $Tuple 0))
  (func
    $mul
    (param $v1 (ref $Tuple_1))
    (param $v2 (ref $Tuple_1))
    (result (ref $Tuple_1))
    (local.get $v1)
    (struct.get $Tuple_1 0)
    (local.get $v2)
    (struct.get $Tuple_1 0)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_1 1)
    (local.get $v2)
    (struct.get $Tuple_1 1)
    i32.mul
    (local.get $v1)
    (struct.get $Tuple_1 2)
    (local.get $v2)
    (struct.get $Tuple_1 2)
    i32.mul
    (struct.new $Tuple_1))
  (func
    $mul-scalar
    (param $v (ref $Tuple_1))
    (param $s i32)
    (result (ref $Tuple_1))
    (local.get $v)
    (struct.get $Tuple_1 0)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_1 1)
    (local.get $s)
    i32.mul
    (local.get $v)
    (struct.get $Tuple_1 2)
    (local.get $s)
    i32.mul
    (struct.new $Tuple_1))
  (func
    $nested
    (result (ref $Tuple_3))
    (i32.const 33)
    (i32.const 1)
    (call $pair)
    (struct.new $Tuple_2)
    (i32.const 0)
    (struct.new $Tuple_3))
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