define i32 @caller() {
entry:
  %result = call i32 @hof(i32(i32)* @inc)
  ret i32 %result
}

define i32 @hof(i32(i32)* %f) {
entry:
  %arg = call i32 %f(i32 42)
  %result = add i32 10, %arg
  ret i32 %result
}

define i32 @inc(i32 %i) {
entry:
  %result = add i32 %i, 1
  ret i32 %result
}
