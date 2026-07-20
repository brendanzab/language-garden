define i32(i32)* @choose(i1 %b, i32(i32)* %f, i32(i32)* %g) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  br label %if_end
if_end:
  %result = phi i32(i32)* [%f, %if_true], [%g, %if_false]
  ret i32(i32)* %result
}

define i32 @decr(i32 %i) {
entry:
  %result = sub i32 %i, 1
  ret i32 %result
}

define i32 @incr(i32 %i) {
entry:
  %result = add i32 %i, 1
  ret i32 %result
}

define i32(i32)* @partial-app() {
entry:
  %result = call i32(i32)* @choose(i1 true, i32(i32)* @incr, i32(i32)* @decr)
  ret i32(i32)* %result
}

define i32 @test-false() {
entry:
  %fun = call i32(i32)* @choose(i1 false, i32(i32)* @incr, i32(i32)* @decr)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-partial-app() {
entry:
  %fun = call i32(i32)* @partial-app()
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-true() {
entry:
  %fun = call i32(i32)* @choose(i1 true, i32(i32)* @incr, i32(i32)* @decr)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}
