define i32 @fact(i32 %n) {
  %cond = icmp eq i32 %n 0
  br i1 %cond label %if_true, label %if_false
if_true:
  br label %if_end
if_false:
  %arg = sub i32 %n 1
  %arg_1 = call i32 @fact(i32 %arg)
  %false_result = mul i32 %n %arg_1
  br label %if_end
if_end:
  %result = phi i32 [1, %if_true], [%false_result, %if_false]
  ret i32 %result
}

define i32 @test-fact() {
  %result = call i32 @fact(i32 5)
  ret i32 %result
}
