; Hello world, using libc's `puts` function
;
; - https://en.cppreference.com/c/io/puts
; - https://linux.die.net/man/3/puts

declare void @puts(ptr)

; The message to write to stdout. '\00' is the null-terminator, which is
; required when using `puts`.
@.greeting = private constant [14 x i8] c"Hello, world!\00"

define i32 @main() {
  call void @puts(ptr @.greeting) ; write the message to stdout
  ret i32 0
}
