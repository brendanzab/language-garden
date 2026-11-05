; Example of using libc’s `malloc` to dynamically allocate memory
;
; https://en.cppreference.com/c/program/malloc

; NOTE: this could break on 32 bit platforms
declare ptr @malloc(i64)

define ptr @point16-new(i16 %x, i16 %y) {
  %p = call ptr @malloc(i64 4)
  %x-dst = getelementptr i8, ptr %p, i32 0
  %y-dst = getelementptr i8, ptr %p, i32 2
  store i16 %x, ptr %x-dst
  store i16 %y, ptr %y-dst
  ret ptr %p
}

define i16 @point16-x(ptr %p) {
  %x-src = getelementptr i8, ptr %p, i32 0
  %x = load i16, ptr %x-src
  ret i16 %x
}

define i16 @point16-y(ptr %p) {
  %y-src = getelementptr i8, ptr %p, i32 2
  %y = load i16, ptr %y-src
  ret i16 %y
}

define ptr @point16-scale(ptr %p, i16 %factor) {
  %old-x = call i16 @point16-x(ptr %p)
  %old-y = call i16 @point16-y(ptr %p)
  %x = mul i16 %old-x, %factor
  %y = mul i16 %old-y, %factor
  %new-p = call ptr @point16-new(i16 %x, i16 %y)
  ret ptr %new-p
}

define void @assert(i1 %b) {
  br i1 %b, label %if_true, label %if_false
if_true:
  ret void
if_false:
  call void @llvm.trap()    ; Abort the program
  unreachable
}

define i32 @main() {
  %p = call ptr @point16-new(i16 4, i16 5)
  %p-scaled = call ptr @point16-scale(ptr %p, i16 3)

  %x = call i16 @point16-x(ptr %p-scaled)
  %y = call i16 @point16-y(ptr %p-scaled)

  %x-is-12 = icmp eq i16 %x, 12
  %y-is-15 = icmp eq i16 %y, 15
  call void @assert(i1 %x-is-12)
  call void @assert(i1 %y-is-15)

  ret i32 0
}
