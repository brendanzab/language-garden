define i32 @ackermann(i32 %m, i32 %n) {
entry:
  %cond = icmp eq i32 %m, 0
  br i1 %cond, label %if_true, label %if_false
if_true_1:
  %arg_3 = sub i32 %m, 1
  %result_3 = call i32 @ackermann(i32 %arg_3, i32 1)
  br label %if_end_1
if_false_1:
  %arg = sub i32 %m, 1
  %arg_1 = sub i32 %n, 1
  %arg_2 = call i32 @ackermann(i32 %m, i32 %arg_1)
  %result_2 = call i32 @ackermann(i32 %arg, i32 %arg_2)
  br label %if_end_1
if_true:
  %result_4 = add i32 %n, 1
  br label %if_end
if_false:
  %cond_1 = icmp eq i32 %n, 0
  br i1 %cond_1, label %if_true_1, label %if_false_1
if_end:
  %result = phi i32 [%result_4, %if_true], [%result_1, %if_end_1]
  ret i32 %result
if_end_1:
  %result_1 = phi i32 [%result_3, %if_true_1], [%result_2, %if_false_1]
  br label %if_end
}
