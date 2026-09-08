(module
  (export "test-add" (func $test-add))
  (export "test-equal" (func $test-equal))
  (export "test-thunk" (func $test-thunk))
  (type $funty (func (param i32 i32) (result i32)))
  (type $funty_1 (func (param ) (result i32)))
  (elem declare func $add $equal $forty-two)
  (func
    $add
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.add)
  (func
    $apply2
    (param $f (ref $funty))
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    (local.get $f)
    (return_call_ref $funty))
  (func
    $compare
    (param $f (ref $funty))
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    (local.get $f)
    (return_call_ref $funty))
  (func
    $equal
    (param $x i32)
    (param $y i32)
    (result i32)
    (local.get $x)
    (local.get $y)
    i32.eq)
  (func $forty-two (result i32) (i32.const 42))
  (func
    $test-add
    (result i32)
    (ref.func $add)
    (i32.const 1)
    (i32.const 2)
    (return_call $apply2))
  (func
    $test-equal
    (result i32)
    (ref.func $equal)
    (i32.const 1)
    (i32.const 2)
    (return_call $compare))
  (func $test-thunk (result i32) (ref.func $forty-two) (return_call $thunk))
  (func
    $thunk
    (param $f (ref $funty_1))
    (result i32)
    (local.get $f)
    (return_call_ref $funty_1)))