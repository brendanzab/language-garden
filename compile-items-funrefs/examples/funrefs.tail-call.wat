(module
  (export "choose" (func $choose))
  (export "decr" (func $decr))
  (export "incr" (func $incr))
  (export "partial-app" (func $partial-app))
  (export "test-false" (func $test-false))
  (export "test-partial-app" (func $test-partial-app))
  (export "test-true" (func $test-true))
  (type $funty (func (param i32) (result i32)))
  (elem declare func $incr $decr)
  (func
    $choose
    (param $b i32)
    (param $f (ref $funty))
    (param $g (ref $funty))
    (result (ref $funty))
    (local.get $b)
    (if (result (ref $funty)) (then (local.get $f)) (else (local.get $g))))
  (func
    $decr
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.sub)
  (func
    $incr
    (param $i i32)
    (result i32)
    (local.get $i)
    (i32.const 1)
    i32.add)
  (func
    $partial-app
    (result (ref $funty))
    (i32.const 1)
    (ref.func $incr)
    (ref.func $decr)
    (return_call $choose))
  (func
    $test-false
    (result i32)
    (i32.const 42)
    (i32.const 0)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose)
    (return_call_ref $funty))
  (func
    $test-partial-app
    (result i32)
    (i32.const 42)
    (call $partial-app)
    (return_call_ref $funty))
  (func
    $test-true
    (result i32)
    (i32.const 42)
    (i32.const 1)
    (ref.func $incr)
    (ref.func $decr)
    (call $choose)
    (return_call_ref $funty)))