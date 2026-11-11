define i32 @nested-if(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  %cond_1 = icmp eq i32 %y, 0
  br i1 %cond_1, label %if_true_1, label %if_false_1
if_true_1:
  br label %if_end_1
if_false_1:
  %false_result = add i32 %y, 100
  br label %if_end_1
if_end_1:
  %arg = phi i32 [1000000000, %if_true_1], [%false_result, %if_false_1]
  %true_result = add i32 %arg, 3
  br label %if_end
if_false:
  %false_result_1 = add i32 %x, 100
  br label %if_end
if_end:
  %arg_1 = phi i32 [%true_result, %if_end_1], [%false_result_1, %if_false]
  %result = add i32 %arg_1, 4
  ret i32 %result
}
