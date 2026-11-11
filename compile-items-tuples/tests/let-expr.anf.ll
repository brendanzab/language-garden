define i32 @test(i1 %b) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  %result_2 = add i32 42, 1
  br label %if_end
if_false:
  %result_1_1 = sub i32 42, 1
  br label %if_end
if_end:
  %result = phi i32 [%result_2, %if_true], [%result_1_1, %if_false]
  %result_1 = add i32 %result, 42
  ret i32 %result_1
}

define i32 @test-branches(i1 %b) {
entry:
  br i1 %b, label %if_true, label %if_false
if_true:
  %result_2 = add i32 42, 1
  br label %if_end
if_false:
  %result_1 = sub i32 3, 1
  br label %if_end
if_end:
  %result = phi i32 [%result_2, %if_true], [%result_1, %if_false]
  ret i32 %result
}

define i32 @test-shadow(i32 %x) {
entry:
  %x_1 = add i32 %x, 1
  %x_2 = mul i32 %x_1, 3
  ret i32 %x_2
}
