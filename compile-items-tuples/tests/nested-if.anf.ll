define i32 @nested-if(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %if_true, label %if_false
if_true_1:
  br label %if_end_1
if_false_1:
  %result_3 = add i32 %y, 100
  br label %if_end_1
if_true:
  %cond_1 = icmp eq i32 %y, 0
  br i1 %cond_1, label %if_true_1, label %if_false_1
if_false:
  %result_1_1 = add i32 %x, 100
  br label %if_end
if_end:
  %result = phi i32 [%result_4, %if_end_1], [%result_1_1, %if_false]
  %result_1 = add i32 %result, 4
  ret i32 %result_1
if_end_1:
  %result_2 = phi i32 [1000000000, %if_true_1], [%result_3, %if_false_1]
  %result_4 = add i32 %result_2, 3
  br label %if_end
}
