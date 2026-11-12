declare ptr @malloc(i32)

define private i32 @fst(ptr %p) {
entry:
  %result.ptr = getelementptr i8, ptr %p, i32 0
  %result = load i32, ptr %result.ptr
  ret i32 %result
}

define private i1 @lookup-nested() {
entry:
  %tuple = call ptr @nested()
  %tuple.ptr = getelementptr i8, ptr %tuple, i32 12
  %tuple_1 = load ptr, ptr %tuple.ptr
  %tuple.ptr_1 = getelementptr i8, ptr %tuple_1, i32 0
  %tuple_2 = load ptr, ptr %tuple.ptr_1
  %result.ptr = getelementptr i8, ptr %tuple_2, i32 1
  %result = load i1, ptr %result.ptr
  ret i1 %result
}

define private ptr @mul(ptr %v1, ptr %v2) {
entry:
  %result = call ptr @malloc(i32 12)
  %arg.ptr = getelementptr i8, ptr %v1, i32 0
  %arg = load i32, ptr %arg.ptr
  %arg.ptr_1 = getelementptr i8, ptr %v2, i32 0
  %arg_1 = load i32, ptr %arg.ptr_1
  %elem = mul i32 %arg, %arg_1
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 %elem, ptr %elem.dst
  %arg.ptr_2 = getelementptr i8, ptr %v1, i32 4
  %arg_2 = load i32, ptr %arg.ptr_2
  %arg.ptr_3 = getelementptr i8, ptr %v2, i32 4
  %arg_3 = load i32, ptr %arg.ptr_3
  %elem_1 = mul i32 %arg_2, %arg_3
  %elem.dst_1 = getelementptr i8, ptr %result, i32 4
  store i32 %elem_1, ptr %elem.dst_1
  %arg.ptr_4 = getelementptr i8, ptr %v1, i32 8
  %arg_4 = load i32, ptr %arg.ptr_4
  %arg.ptr_5 = getelementptr i8, ptr %v2, i32 8
  %arg_5 = load i32, ptr %arg.ptr_5
  %elem_2 = mul i32 %arg_4, %arg_5
  %elem.dst_2 = getelementptr i8, ptr %result, i32 8
  store i32 %elem_2, ptr %elem.dst_2
  ret ptr %result
}

define private ptr @mul-scalar(ptr %v, i32 %s) {
entry:
  %result = call ptr @malloc(i32 12)
  %arg.ptr = getelementptr i8, ptr %v, i32 0
  %arg = load i32, ptr %arg.ptr
  %elem = mul i32 %arg, %s
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 %elem, ptr %elem.dst
  %arg.ptr_1 = getelementptr i8, ptr %v, i32 4
  %arg_1 = load i32, ptr %arg.ptr_1
  %elem_1 = mul i32 %arg_1, %s
  %elem.dst_1 = getelementptr i8, ptr %result, i32 4
  store i32 %elem_1, ptr %elem.dst_1
  %arg.ptr_2 = getelementptr i8, ptr %v, i32 8
  %arg_2 = load i32, ptr %arg.ptr_2
  %elem_2 = mul i32 %arg_2, %s
  %elem.dst_2 = getelementptr i8, ptr %result, i32 8
  store i32 %elem_2, ptr %elem.dst_2
  ret ptr %result
}

define private ptr @nested() {
entry:
  %result = call ptr @malloc(i32 17)
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 33, ptr %elem.dst
  %elem.dst_1 = getelementptr i8, ptr %result, i32 4
  store i32 4, ptr %elem.dst_1
  %elem = call ptr @malloc(i32 9)
  %elem_1 = call ptr @pair()
  %elem.dst_2 = getelementptr i8, ptr %elem, i32 0
  store ptr %elem_1, ptr %elem.dst_2
  %elem.dst_3 = getelementptr i8, ptr %elem, i32 1
  store i1 true, ptr %elem.dst_3
  %elem.dst_4 = getelementptr i8, ptr %result, i32 12
  store ptr %elem, ptr %elem.dst_4
  %elem.dst_5 = getelementptr i8, ptr %result, i32 13
  store i1 false, ptr %elem.dst_5
  ret ptr %result
}

define private ptr @pair() {
entry:
  %result = call ptr @malloc(i32 5)
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 33, ptr %elem.dst
  %elem.dst_1 = getelementptr i8, ptr %result, i32 1
  store i1 false, ptr %elem.dst_1
  ret ptr %result
}

define private ptr @singleton() {
entry:
  %result = call ptr @malloc(i32 4)
  %elem.dst = getelementptr i8, ptr %result, i32 0
  store i32 33, ptr %elem.dst
  ret ptr %result
}

define private i32 @snd(ptr %p) {
entry:
  %result.ptr = getelementptr i8, ptr %p, i32 4
  %result = load i32, ptr %result.ptr
  ret i32 %result
}

define private ptr @unit() {
entry:
  %result = call ptr @malloc(i32 0)
  ret ptr %result
}
