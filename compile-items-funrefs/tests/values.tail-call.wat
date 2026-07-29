(module
  (export "bar" (func $bar))
  (export "foo" (func $foo))
  (func $bar (result i32) (call $foo) (i32.const 3) i32.mul)
  (func $foo (result i32) (i32.const 42) (i32.const 3) i32.add))