define private i1 @eq-chain(i32 %x) {
entry:
  %arg = icmp eq i32 %x, 1
  %result = icmp eq i1 %arg, false
  ret i1 %result
}
