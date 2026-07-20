(module
  (export "caller" (func $caller))
  (export "hof" (func $hof))
  (export "inc" (func $inc))
  (type $funty (func (param i32) (result i32)))
  (elem declare func $inc)
  (func $caller (result i32) (ref.func $inc) (return_call $hof))
  (func
    $hof
    (param $f (ref $funty))
    (result i32)
    (i32.const 10)
    (i32.const 42)
    (local.get $f)
    (call_ref $funty)
    i32.add)
  (func
    $inc
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.add))