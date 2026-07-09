define i32 @test(i1 %b) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  %true_result = add i32 42, 1
  br label %if_true_end
if_false:
  %false_result = sub i32 42, 1
  br label %if_false_end
if_true_end:
  br label %if_end
if_false_end:
  br label %if_end
if_end:
  %y = phi i32 [%true_result, %if_true_end], [%false_result, %if_false_end]
  %result = add i32 %y, 42
  ret i32 %result
}

define i32 @test-branches(i1 %b) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  %true_result = add i32 42, 1
  br label %if_true_end
if_false:
  %false_result = sub i32 3, 1
  br label %if_false_end
if_true_end:
  br label %if_end
if_false_end:
  br label %if_end
if_end:
  %result =
    phi i32 [%true_result, %if_true_end], [%false_result, %if_false_end]
  ret i32 %result
}

define i32 @test-shadow(i32 %x) {
entry:
  %x_1 = add i32 %x, 1
  %x_2 = mul i32 %x_1, 3
  ret i32 %x_2
}
