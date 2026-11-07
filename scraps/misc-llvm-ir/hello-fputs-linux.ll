; Hello world, using libc's `fputs` function.
;
; It's probably better to use the `write` syscall for printing to specific
; output streams as `stdout`, `stderr`, and `stdin` are defined as macros, and
; are not easily bound as globals in a platform-agnostic way.
;
; - https://en.cppreference.com/c/io/fputs
; - https://linux.die.net/man/3/fputs
; - https://en.cppreference.com/c/io/std_streams
; - https://github.com/austral/austral/blob/0962d2a8a5d77f7daacd7f696819520733f4897d/lib/prelude.c#L56-L90

; This probably only works on linux-like systems.
@stdout = external global ptr

declare void @fputs(ptr, ptr)

; The message to write to stdout. '\00' is the null-terminator, which is
; required when using `puts`.
@.greeting = private constant [14 x i8] c"Hello, world!\00"

define i32 @main() {
  %stdout = load ptr, ptr @stdout
  call void @fputs(ptr @.greeting, ptr %stdout) ; write the message to stdout
  ret i32 0
}
