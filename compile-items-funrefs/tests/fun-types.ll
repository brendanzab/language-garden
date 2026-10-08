define private i32 @call-param(ptr %f) {
entry:
  %result = call i32 %f(i32 43, i1 true)
  ret i32 %result
}

define private i1 @fun-app(ptr %f, i32 %x) {
entry:
  %result = call i1 %f(i32 %x)
  ret i1 %result
}

define private ptr @id-i32-bool(ptr %f) {
entry:
  ret ptr %f
}

define private i32 @ignore-param(ptr %_) {
entry:
  ret i32 43
}

define private i1 @is-one(i32 %x) {
entry:
  %result = icmp eq i32 %x, 1
  ret i1 %result
}

define private i1 @is-zero(i32 %x) {
entry:
  %result = icmp eq i32 %x, 0
  ret i1 %result
}

define private i32 @local-binding(ptr %f, i32 %x) {
entry:
  %result = call i32 %f(i32 %x)
  ret i32 %result
}

define i1 @test-hof-1() {
entry:
  %result = call i1 @fun-app(ptr @is-zero, i32 3)
  ret i1 %result
}

define i1 @test-hof-2() {
entry:
  %result = call i1 @fun-app(ptr @is-zero, i32 45)
  ret i1 %result
}

define i1 @test-hof-3() {
entry:
  %result = call i1 @fun-app(ptr @is-one, i32 1)
  ret i1 %result
}
