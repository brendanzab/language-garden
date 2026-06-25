define i32 @add_one(i32 %x) {
entry:
  %result = add i32 %x, 1
  ret i32 %result
}

define i32 @test(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_true_end
if_false:
  br label %if_false_end
if_true_end:
  br label %if_end
if_false_end:
  br label %if_end
if_end:
  %arg = phi i32 [%y, %if_true_end], [%x, %if_false_end]
  %result = add i32 %arg, 1
  ret i32 %result
}

define i32 @test-tailcall(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_true_end
if_false:
  br label %if_false_end
if_true_end:
  br label %if_end
if_false_end:
  br label %if_end
if_end:
  %arg = phi i32 [%x, %if_true_end], [%y, %if_false_end]
  %result = call i32 @add_one(i32 %arg)
  ret i32 %result
}
