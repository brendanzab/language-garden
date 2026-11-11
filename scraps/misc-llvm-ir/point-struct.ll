; Example of using libc’s `malloc` to dynamically allocate memory, using LLVM’s
; struct types. Note that most of these uses of typed pointers are not actually
; type checked these days. This is because LLVM is moving to opaque pointers
; (see: https://llvm.org/docs/OpaquePointers.html). The `point-opaque.ll` file
; shows an alternative implementation.
;
; - https://en.cppreference.com/c/program/malloc

%Point = type { i16, i16 }

; NOTE: this could break on 32 bit platforms
declare ptr @malloc(i64)

define %Point* @point16-new(i16 %x, i16 %y) {
  ; Hack to compute the size of the struct in a portable way
  ;
  ; Pretend that there is an array of elements at the null pointer. Compute the
  ; pointer to the first element of that array then cast it to an integer.
  ;
  ; - https://nondot.org/sabre/LLVMNotes/SizeOf-OffsetOf-VariableSizedStructs.txt
  ; - https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl10.html#implementing-portable-offsetof-sizeof
  ; - https://llvm.org/doxygen/classllvm_1_1ConstantExpr.html#a778163e6ec80716a12ab3282cb97f0d9
  %offset = getelementptr %Point, ptr null, i32 1
  %size = ptrtoint ptr %offset to i64

  %p = call ptr @malloc(i64 %size)
  %x-dst = getelementptr %Point, ptr %p, i32 0, i32 0
  %y-dst = getelementptr %Point, ptr %p, i32 0, i32 1
  store i16 %x, ptr %x-dst
  store i16 %y, ptr %y-dst
  ret %Point* %p
}

define i16 @point16-x(%Point* %p) {
  %x-src = getelementptr %Point, ptr %p, i32 0, i32 0
  %x = load i16, ptr %x-src
  ret i16 %x
}

define i16 @point16-y(%Point* %p) {
  %y-src = getelementptr %Point, ptr %p, i32 0, i32 1
  %y = load i16, ptr %y-src
  ret i16 %y
}

define %Point* @point16-scale(%Point* %p, i16 %factor) {
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
