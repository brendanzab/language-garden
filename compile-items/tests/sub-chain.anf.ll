define private i32 @sub-chain(i32 %x) {
entry:
  %arg = sub i32 %x, 1
  %result = sub i32 %arg, 2
  ret i32 %result
}
