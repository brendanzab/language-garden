define i1 @bool-eq-op(i1 %x, i1 %y) {
entry:
  %result = icmp eq i1 %x, %y
  ret i1 %result
}

define i1 @bool-eq-prim(i1 %x, i1 %y) {
entry:
  %result = icmp eq i1 %x, %y
  ret i1 %result
}

define i32 @i32-add-op(i32 %x, i32 %y) {
entry:
  %result = add i32 %x, %y
  ret i32 %result
}

define i32 @i32-add-prim(i32 %x, i32 %y) {
entry:
  %result = add i32 %x, %y
  ret i32 %result
}

define i1 @i32-eq-op(i32 %x, i32 %y) {
entry:
  %result = icmp eq i32 %x, %y
  ret i1 %result
}

define i1 @i32-eq-prim(i32 %x, i32 %y) {
entry:
  %result = icmp eq i32 %x, %y
  ret i1 %result
}

define i32 @i32-mul-op(i32 %x, i32 %y) {
entry:
  %result = mul i32 %x, %y
  ret i32 %result
}

define i32 @i32-mul-prim(i32 %x, i32 %y) {
entry:
  %result = mul i32 %x, %y
  ret i32 %result
}

define i32 @i32-sub-op(i32 %x, i32 %y) {
entry:
  %result = sub i32 %x, %y
  ret i32 %result
}

define i32 @i32-sub-prim(i32 %x, i32 %y) {
entry:
  %result = sub i32 %x, %y
  ret i32 %result
}

define i32 @neg-op(i32 %x) {
entry:
  %result = sub i32 0, %x
  ret i32 %result
}

define i32 @neg-prim(i32 %x) {
entry:
  %result = sub i32 0, %x
  ret i32 %result
}
