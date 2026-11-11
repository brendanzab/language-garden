declare ptr @malloc(i32)

define private i32 @fst(ptr %p) {
entry:
  %result.ptr = getelementptr {i32, i32}, ptr %p, i32 0, i32 0
  %result = load i32, ptr %result.ptr
  ret i32 %result
}

define private ptr @mul(ptr %v1, ptr %v2) {
entry:
  %size.offset = getelementptr {i32, i32, i32}, ptr null, i32 1
  %size = ptrtoint ptr %size.offset to i32
  %result = call ptr @malloc(i32 %size)
  %arg.ptr = getelementptr {i32, i32, i32}, ptr %v1, i32 0, i32 0
  %arg = load i32, ptr %arg.ptr
  %arg.ptr_1 = getelementptr {i32, i32, i32}, ptr %v2, i32 0, i32 0
  %arg_1 = load i32, ptr %arg.ptr_1
  %elem = mul i32 %arg, %arg_1
  %elem.dst = getelementptr {i32, i32, i32}, ptr %result, i32 0, i32 0
  store i32 %elem, ptr %elem.dst
  %arg.ptr_2 = getelementptr {i32, i32, i32}, ptr %v1, i32 0, i32 1
  %arg_2 = load i32, ptr %arg.ptr_2
  %arg.ptr_3 = getelementptr {i32, i32, i32}, ptr %v2, i32 0, i32 1
  %arg_3 = load i32, ptr %arg.ptr_3
  %elem_1 = mul i32 %arg_2, %arg_3
  %elem.dst_1 = getelementptr {i32, i32, i32}, ptr %result, i32 0, i32 1
  store i32 %elem_1, ptr %elem.dst_1
  %arg.ptr_4 = getelementptr {i32, i32, i32}, ptr %v1, i32 0, i32 2
  %arg_4 = load i32, ptr %arg.ptr_4
  %arg.ptr_5 = getelementptr {i32, i32, i32}, ptr %v2, i32 0, i32 2
  %arg_5 = load i32, ptr %arg.ptr_5
  %elem_2 = mul i32 %arg_4, %arg_5
  %elem.dst_2 = getelementptr {i32, i32, i32}, ptr %result, i32 0, i32 2
  store i32 %elem_2, ptr %elem.dst_2
  ret ptr %result
}

define private ptr @mul-scalar(ptr %v, i32 %s) {
entry:
  %size.offset = getelementptr {i32, i32, i32}, ptr null, i32 1
  %size = ptrtoint ptr %size.offset to i32
  %result = call ptr @malloc(i32 %size)
  %arg.ptr = getelementptr {i32, i32, i32}, ptr %v, i32 0, i32 0
  %arg = load i32, ptr %arg.ptr
  %elem = mul i32 %arg, %s
  %elem.dst = getelementptr {i32, i32, i32}, ptr %result, i32 0, i32 0
  store i32 %elem, ptr %elem.dst
  %arg.ptr_1 = getelementptr {i32, i32, i32}, ptr %v, i32 0, i32 1
  %arg_1 = load i32, ptr %arg.ptr_1
  %elem_1 = mul i32 %arg_1, %s
  %elem.dst_1 = getelementptr {i32, i32, i32}, ptr %result, i32 0, i32 1
  store i32 %elem_1, ptr %elem.dst_1
  %arg.ptr_2 = getelementptr {i32, i32, i32}, ptr %v, i32 0, i32 2
  %arg_2 = load i32, ptr %arg.ptr_2
  %elem_2 = mul i32 %arg_2, %s
  %elem.dst_2 = getelementptr {i32, i32, i32}, ptr %result, i32 0, i32 2
  store i32 %elem_2, ptr %elem.dst_2
  ret ptr %result
}

define private ptr @nested() {
entry:
  %size.offset = getelementptr {i32, ptr, i1}, ptr null, i32 1
  %size = ptrtoint ptr %size.offset to i32
  %result = call ptr @malloc(i32 %size)
  %elem.dst = getelementptr {i32, ptr, i1}, ptr %result, i32 0, i32 0
  store i32 33, ptr %elem.dst
  %size.offset_1 = getelementptr {i1, ptr}, ptr null, i32 1
  %size_1 = ptrtoint ptr %size.offset_1 to i32
  %elem = call ptr @malloc(i32 %size_1)
  %elem.dst_1 = getelementptr {i1, ptr}, ptr %elem, i32 0, i32 0
  store i1 true, ptr %elem.dst_1
  %elem_1 = call ptr @pair()
  %elem.dst_2 = getelementptr {i1, ptr}, ptr %elem, i32 0, i32 1
  store ptr %elem_1, ptr %elem.dst_2
  %elem.dst_3 = getelementptr {i32, ptr, i1}, ptr %result, i32 0, i32 1
  store ptr %elem, ptr %elem.dst_3
  %elem.dst_4 = getelementptr {i32, ptr, i1}, ptr %result, i32 0, i32 2
  store i1 false, ptr %elem.dst_4
  ret ptr %result
}

define private ptr @pair() {
entry:
  %size.offset = getelementptr {i32, i1}, ptr null, i32 1
  %size = ptrtoint ptr %size.offset to i32
  %result = call ptr @malloc(i32 %size)
  %elem.dst = getelementptr {i32, i1}, ptr %result, i32 0, i32 0
  store i32 33, ptr %elem.dst
  %elem.dst_1 = getelementptr {i32, i1}, ptr %result, i32 0, i32 1
  store i1 false, ptr %elem.dst_1
  ret ptr %result
}

define private ptr @singleton() {
entry:
  %size.offset = getelementptr {i32}, ptr null, i32 1
  %size = ptrtoint ptr %size.offset to i32
  %result = call ptr @malloc(i32 %size)
  %elem.dst = getelementptr {i32}, ptr %result, i32 0, i32 0
  store i32 33, ptr %elem.dst
  ret ptr %result
}

define private i32 @snd(ptr %p) {
entry:
  %result.ptr = getelementptr {i32, i32}, ptr %p, i32 0, i32 1
  %result = load i32, ptr %result.ptr
  ret i32 %result
}

define private ptr @unit() {
entry:
  %size.offset = getelementptr {}, ptr null, i32 1
  %size = ptrtoint ptr %size.offset to i32
  %result = call ptr @malloc(i32 %size)
  ret ptr %result
}
