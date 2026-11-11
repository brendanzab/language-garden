define private i32 @add_one(i32 %x) {
entry:
  %result = add i32 %x, 1
  ret i32 %result
}

define i32 @test(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  br label %if_end
if_end:
  %result = phi i32 [%y, %if_true], [%x, %if_false]
  %result_1 = add i32 %result, 1
  ret i32 %result_1
}

define i32 @test-tailcall(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  br label %if_end
if_end:
  %result = phi i32 [%x, %if_true], [%y, %if_false]
  %result_1 = call i32 @add_one(i32 %result)
  ret i32 %result_1
}
