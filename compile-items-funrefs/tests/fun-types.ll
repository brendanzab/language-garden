define i1 @fun-app(i1(i32)* %f, i32 %x) {
entry:
  %result = call i1 %f(i32 %x)
  ret i1 %result
}

define i1(i32)* @id-i32-bool(i1(i32)* %f) {
entry:
  ret i1(i32)* %f
}

define i32 @ignore-param(i32(i32, i1)* %_) {
entry:
  ret i32 43
}

define i1 @is-one(i32 %x) {
entry:
  %result = icmp eq i32 %x, 1
  ret i1 %result
}

define i1 @is-zero(i32 %x) {
entry:
  %result = icmp eq i32 %x, 0
  ret i1 %result
}

define i32 @local-binding(i32(i32)* %f, i32 %x) {
entry:
  %result = call i32 %f(i32 %x)
  ret i32 %result
}

define i1 @test-hof-1() {
entry:
  %result = call i1 @fun-app(i1(i32)* @is-zero, i32 3)
  ret i1 %result
}

define i1 @test-hof-2() {
entry:
  %result = call i1 @fun-app(i1(i32)* @is-zero, i32 45)
  ret i1 %result
}

define i1 @test-hof-3() {
entry:
  %result = call i1 @fun-app(i1(i32)* @is-one, i32 1)
  ret i1 %result
}
