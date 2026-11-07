; Example of implementing assert with the `@llvm.trap` intrinsic
;
; - https://llvm.org/docs/LangRef.html#llvm-trap-intrinsic

define i32 @square(i32) local_unnamed_addr #0 {
  %2 = mul nsw i32 %0, %0
  ret i32 %2
}

define void @assert(i1 %b) {
  br i1 %b, label %if_true, label %if_false
if_true:
  ret void
if_false:
  call void @llvm.trap()    ; Abort the program
  unreachable
}

define i32 @main() {
  %result = call i32 @square(i32 2)
  %result_is_4 = icmp eq i32 %result, 4
  call void @assert(i1 %result_is_4)
  ret i32 0
}
