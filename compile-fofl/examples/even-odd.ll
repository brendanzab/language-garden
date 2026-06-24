define i1 @is-even(i32 %n) {
  %cond = icmp eq i32 %n, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  %arg = sub i32 %n, 1
  %false_result = call i1 @is-odd(i32 %arg)
  br label %if_end
if_end:
  %result = phi i1 [true, %if_true], [%false_result, %if_false]
  ret i1 %result
}

define i1 @is-odd(i32 %n) {
  %cond = icmp eq i32 %n, 0
  br i1 %cond, label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  %arg = sub i32 %n, 1
  %false_result = call i1 @is-even(i32 %arg)
  br label %if_end
if_end:
  %result = phi i1 [false, %if_true], [%false_result, %if_false]
  ret i1 %result
}
