define private ptr @choose(i1 %b, ptr %f, ptr %g) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  br label %if_end
if_end:
  %result = phi ptr [%f, %if_true], [%g, %if_false]
  ret ptr %result
}

define i32 @decr(i32 %i) {
entry:
  %result = sub i32 %i, 1
  ret i32 %result
}

define private i32 @decr2(i32 %i) {
entry:
  %result = sub i32 %i, 1
  ret i32 %result
}

define i32 @incr(i32 %i) {
entry:
  %result = add i32 %i, 1
  ret i32 %result
}

define private i32 @incr2(i32 %i) {
entry:
  %result = add i32 %i, 1
  ret i32 %result
}

define private ptr @partial-app() {
entry:
  %result = call ptr @choose(i1 true, ptr @incr, ptr @decr)
  ret ptr %result
}

define i32 @test-false() {
entry:
  %fun = call ptr @choose(i1 false, ptr @incr, ptr @decr)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-false-2() {
entry:
  %fun = call ptr @choose(i1 false, ptr @incr2, ptr @decr2)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-local-def() {
entry:
  %partial-app = call ptr @choose(i1 true, ptr @incr, ptr @decr)
  %fun = call ptr @partial-app()
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-partial-app() {
entry:
  %fun = call ptr @partial-app()
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-true() {
entry:
  %fun = call ptr @choose(i1 true, ptr @incr, ptr @decr)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}

define i32 @test-true-2() {
entry:
  %fun = call ptr @choose(i1 true, ptr @incr2, ptr @decr2)
  %result = call i32 %fun(i32 42)
  ret i32 %result
}
