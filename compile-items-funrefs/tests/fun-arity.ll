define private i32 @add(i32 %x, i32 %y) {
entry:
  %result = add i32 %x, %y
  ret i32 %result
}

define private i32 @apply2(i32(i32, i32)* %f, i32 %x, i32 %y) {
entry:
  %result = call i32 %f(i32 %x, i32 %y)
  ret i32 %result
}

define private i1 @compare(i1(i32, i32)* %f, i32 %x, i32 %y) {
entry:
  %result = call i1 %f(i32 %x, i32 %y)
  ret i1 %result
}

define private i1 @equal(i32 %x, i32 %y) {
entry:
  %result = icmp eq i32 %x, %y
  ret i1 %result
}

define private i32 @forty-two() {
entry:
  ret i32 42
}

define i32 @test-add() {
entry:
  %result = call i32 @apply2(i32(i32, i32)* @add, i32 1, i32 2)
  ret i32 %result
}

define i1 @test-equal() {
entry:
  %result = call i1 @compare(i1(i32, i32)* @equal, i32 1, i32 2)
  ret i1 %result
}

define i32 @test-thunk() {
entry:
  %result = call i32 @thunk(i32()* @forty-two)
  ret i32 %result
}

define private i32 @thunk(i32()* %f) {
entry:
  %result = call i32 %f()
  ret i32 %result
}
