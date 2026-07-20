(module
  (export "fun-app" (func $fun-app))
  (export "id-i32-bool" (func $id-i32-bool))
  (export "ignore-param" (func $ignore-param))
  (export "is-one" (func $is-one))
  (export "is-zero" (func $is-zero))
  (export "local-binding" (func $local-binding))
  (export "test-hof-1" (func $test-hof-1))
  (export "test-hof-2" (func $test-hof-2))
  (export "test-hof-3" (func $test-hof-3))
  (type $funty (func (param i32) (result i32)))
  (type $funty_1 (func (param i32 i32) (result i32 i32)))
  (elem declare func $is-zero $is-one)
  (func
    $fun-app
    (param $f (ref $funty))
    (param $x i32)
    (result i32)
    (local.get $x)
    (local.get $f)
    (return_call_ref $funty))
  (func
    $id-i32-bool
    (param $f (ref $funty))
    (result (ref $funty))
    (local.get $f))
  (func $ignore-param (param $_ (ref $funty_1)) (result i32) (i32.const 43))
  (func
    $is-one
    (param $x i32)
    (result i32)
    (local.get $x)
    (i32.const 1)
    i32.eq)
  (func
    $is-zero
    (param $x i32)
    (result i32)
    (local.get $x)
    (i32.const 0)
    i32.eq)
  (func
    $local-binding
    (param $f (ref $funty))
    (param $x i32)
    (result i32)
    (local $g (ref $funty))
    (local.get $f)
    (local.set $g)
    (local.get $x)
    (local.get $g)
    (return_call_ref $funty))
  (func
    $test-hof-1
    (result i32)
    (ref.func $is-zero)
    (i32.const 3)
    (return_call $fun-app))
  (func
    $test-hof-2
    (result i32)
    (ref.func $is-zero)
    (i32.const 45)
    (return_call $fun-app))
  (func
    $test-hof-3
    (result i32)
    (ref.func $is-one)
    (i32.const 1)
    (return_call $fun-app)))