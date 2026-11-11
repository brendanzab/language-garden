define i32 @bar() {
entry:
  %arg = call i32 @foo()
  %result = mul i32 %arg, 3
  ret i32 %result
}

define i32 @foo() {
entry:
  %result = add i32 42, 3
  ret i32 %result
}
