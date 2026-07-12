define i32 @neg(i32 %x) {
entry:
  %result = sub i32 0, %x
  ret i32 %result
}

define i32 @neg-expr(i32 %x, i32 %y) {
entry:
  %arg = sub i32 %x, %y
  %result = sub i32 0, %arg
  ret i32 %result
}

define i32 @neg-neg(i32 %x) {
entry:
  %arg = sub i32 0, %x
  %result = sub i32 0, %arg
  ret i32 %result
}

define i32 @neg-prim(i32 %x) {
entry:
  %result = sub i32 0, %x
  ret i32 %result
}
