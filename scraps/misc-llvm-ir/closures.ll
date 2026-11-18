; Demonstration of closures in LLVM IR
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

; Closures

; Data representation:
;
; ```
;     ┌─────┐
;     │  ●  │
;     └──│──┘
;        │
;        ▼
;     ┌─────┬─────┐
;     │  ●  │  ●  │  closure  (dynamically allocated)
;     └──│──┴──│──┘
;        │     │     ┌─────┬ - - ┬─────┐
;        │     └────▶︎│  x₁          xₙ │  env  (dynamically allocated)
;        │           └─────┴ - - ┴─────┘
;        ▼
;      (env, args...) -> ret
; ```

; Allocate space for the closure and store the `code` and `env` in it.
define ptr @clos-new(ptr %code, ptr %env) {
  %clos = call ptr @malloc(i64 16)  ; sizeof(ptr) + sizeof(ptr) = 8 + 8 = 16
  %clos.code.ptr = getelementptr i8, ptr %clos, i32 0
  %clos.env.ptr = getelementptr i8, ptr %clos, i32 8
  store ptr %code, ptr %clos.code.ptr
  store ptr %env, ptr %clos.env.ptr
  ret ptr %clos
}

; Access the code of the closure
define ptr @clos-code(ptr %clos) {
  %code.ptr = getelementptr i8, ptr %clos, i32 0
  %code = load ptr, ptr %code.ptr
  ret ptr %code
}

; Access the environment of the closure
define ptr @clos-env(ptr %clos) {
  %env.ptr = getelementptr i8, ptr %clos, i32 8
  %env = load ptr, ptr %env.ptr
  ret ptr %env
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

define i32 @h.clos.code(ptr %env) {
  ret i32 3
}

define ptr @h(i32 %x) {
  %clos = call ptr @clos-new(ptr @h.clos.code, ptr null)
  ret ptr %clos
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
  ; Allocate space for the environment and store `x` in it
  %g.env = call ptr @malloc(i64 4)   ; sizeof(i32) = 4
  %g.env.x.ptr = getelementptr i8, ptr %g.env, i32 0
  store i32 %x, ptr %g.env.x.ptr

  %g = call ptr @clos-new(ptr @f.g.code, ptr %g.env)
  ret ptr %g
}


; Closures with arguments
;
; ```
; fun curried-add(x : I32) : fun (I32) -> I32 :=
;   fun (y) -> x + y;
; ```

define i32 @curried-add.clos.code(ptr %env, i32 %y) {
  %x.ptr = getelementptr i8, ptr %env, i32 0
  %x = load i32, ptr %x.ptr
  %res = add i32 %x, %y
  ret i32 %res
}

define ptr @curried-add(i32 %x) {
  ; Allocate space for the environment and store `x` in it
  %clos.env = call ptr @malloc(i64 4)   ; sizeof({ i32 x }) = 4
  %clos.env.x.ptr = getelementptr i8, ptr %clos.env, i32 0
  store i32 %x, ptr %clos.env.x.ptr

  %clos = call ptr @clos-new(ptr @curried-add.clos.code, ptr %clos.env)
  ret ptr %clos
}


; Main entrypoint

define i32 @main() {
  ; let () = assert (f(4)() = 4);

  %clos1 = call ptr @f(i32 4)
  %clos1.code = call ptr @clos-code(ptr %clos1)
  %clos1.env = call ptr @clos-env(ptr %clos1)

  %res1 = call i32 %clos1.code(ptr %clos1.env)

  %res1-is-4 = icmp eq i32 %res1, 4
  call void @assert(i1 %res1-is-4)

  ; let () = assert (f()() = 3);

  %clos2 = call ptr @h()
  %clos2.code = call ptr @clos-code(ptr %clos2)
  %clos2.env = call ptr @clos-env(ptr %clos2)

  %res2 = call i32 %clos2.code(ptr %clos2.env)

  %res2-is-3 = icmp eq i32 %res2, 3
  call void @assert(i1 %res2-is-3)

  ; let () = assert (curried-add(3)(5) = 8);

  %clos3 = call ptr @curried-add(i32 3)
  %clos3.code = call ptr @clos-code(ptr %clos3)
  %clos3.env = call ptr @clos-env(ptr %clos3)

  %res3 = call i32 %clos3.code(ptr %clos3.env, i32 5)

  %res3-is-8 = icmp eq i32 %res3, 8
  call void @assert(i1 %res3-is-8)

  ret i32 0
}
