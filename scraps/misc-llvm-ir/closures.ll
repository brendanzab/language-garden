; A demonstration of implementing closures in LLVM IR
;
; This file uses techniques from:
;
; - `./fail.ll`
; - `./point-opaque.ll`
;
; Closure representation:
;
; ```
;     ┌─────┐
;     │  ●  │
;     └──│──┘
;        │
;        ▼
;     ┌─────┬─────┬ - - ┬─────┐
;     │  ●  │  x₁          xₙ │  closure  (dynamically allocated)
;     └──│──┴─────┴ - - ┴─────┘
;        │  └────── env ──────┘
;        │
;        ▼
;      (env, args...) -> ret
; ```
;
; Resources:
;
; - Matt Might. Closure conversion: How to compile lambda.
;   https://matt.might.net/articles/closure-conversion/
; - Mapping High Level Constructs to LLVM IR: Lambda Functions.
;   https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/advanced-constructs/lambda-functions.html


; ------------------------------------------------------------------------------
; Base definitions
; ------------------------------------------------------------------------------

declare ptr @malloc(i64)
declare i32 @write(i32, ptr, i32)
declare void @exit(i32)

; Message to write when failing. '\0A' is the hex code for the '\n' character.
@.fail-msg = private constant [15 x i8] c"program failed\0A"

define void @fail() {
  call i32 @write(i32 2, ptr @.fail-msg, i32 15) ; write failure message to stderr
  call void @exit(i32 1)
  unreachable
}


; ------------------------------------------------------------------------------
; Main code
; ------------------------------------------------------------------------------

; ```
; fun assert(b : Bool) : Unit :=
;   if b then () else fail();
;
; fun f(x : I32) : fun () -> I32 :=
;   let g = fun() -> x;
;   g;
;
; fun h() : fun () -> I32 :=
;   fun() -> 3;
;
; fun curried-add(x : I32) : fun (I32) -> I32 :=
;   fun(y) -> x + y;
;
; fun main() :=
;   let () = assert (f(4)() = 4);
;   let () = assert (f()() = 3);
;   let () = assert (curried-add(3)(5) = 8);
;   ()
; ```


define void @assert(i1 %b) {
  br i1 %b, label %if_true, label %if_false
if_true:
  ret void
if_false:
  call void @fail()
  unreachable
}


; Non-capturing closures
;
; ```
; fun h() : fun () -> I32 :=
;   fun () -> 3;
; ```

define i32 @h.code(ptr %env) {
  ret i32 3
}

define ptr @h() {
  %res = call ptr @malloc(i64 8)   ; sizeof(ptr) = 4
  %res.code.ptr = getelementptr i8, ptr %res, i32 0
  store ptr @h.code, ptr %res.code.ptr
  ret ptr %res
}

; Closures with captures
;
; ```
; fun f(x : I32) : fun () -> I32 :=
;   let g = fun () -> x;
;   g;
; ```

define i32 @f.g.code(ptr %env) {
  %x.ptr = getelementptr i8, ptr %env, i32 0
  %x = load i32, ptr %x.ptr
  ret i32 %x
}

define ptr @f(i32 %x) {
  %g = call ptr @malloc(i64 16)   ; sizeof(ptr) + sizeof(i32) + pad = 8 + 4 + 4
  %g.code.ptr = getelementptr i8, ptr %g, i32 0
  store ptr @f.g.code, ptr %g.code.ptr

  %g.x.ptr = getelementptr i8, ptr %g, i32 8
  store i32 %x, ptr %g.x.ptr

  ret ptr %g
}


; Closures with arguments
;
; ```
; fun curried-add(x : I32) : fun (I32) -> I32 :=
;   fun (y) -> x + y;
; ```

define i32 @curried-add.res.code(ptr %env, i32 %y) {
  %x.ptr = getelementptr i8, ptr %env, i32 0
  %x = load i32, ptr %x.ptr
  %res = add i32 %x, %y
  ret i32 %res
}

define ptr @curried-add(i32 %x) {
  %res = call ptr @malloc(i64 16)   ; sizeof(ptr) + sizeof(i32) + pad = 8 + 4 + 4
  %res.code.ptr = getelementptr i8, ptr %res, i32 0
  store ptr @curried-add.res.code, ptr %res.code.ptr

  %res.x.ptr = getelementptr i8, ptr %res, i32 8
  store i32 %x, ptr %res.x.ptr

  ret ptr %res
}


; Main entrypoint

define i32 @main() {
  ; let () = assert (f(4)() = 4);

  %clos1 = call ptr @f(i32 4)
  %clos1.code.ptr = getelementptr i8, ptr %clos1, i32 0
  %clos1.env.ptr = getelementptr i8, ptr %clos1, i32 8
  %clos1.code = load ptr, ptr %clos1.code.ptr
  %res1 = call i32 %clos1.code(ptr %clos1.env.ptr)

  %res1-is-4 = icmp eq i32 %res1, 4
  call void @assert(i1 %res1-is-4)

  ; let () = assert (h()() = 3);

  %clos2 = call ptr @h()
  %clos2.code.ptr = getelementptr i8, ptr %clos2, i32 0
  %clos2.env.ptr = getelementptr i8, ptr %clos2, i32 8
  %clos2.code = load ptr, ptr %clos2.code.ptr
  %res2 = call i32 %clos2.code(ptr %clos2.env.ptr)

  %res2-is-3 = icmp eq i32 %res2, 3
  call void @assert(i1 %res2-is-3)

  ; let () = assert (curried-add(3)(5) = 8);

  %clos3 = call ptr @curried-add(i32 3)
  %clos3.code.ptr = getelementptr i8, ptr %clos3, i32 0
  %clos3.env.ptr = getelementptr i8, ptr %clos3, i32 8
  %clos3.code = load ptr, ptr %clos3.code.ptr
  %res3 = call i32 %clos3.code(ptr %clos3.env.ptr, i32 5)

  %res3-is-8 = icmp eq i32 %res3, 8
  call void @assert(i1 %res3-is-8)

  ret i32 0
}
