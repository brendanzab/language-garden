; Example of a `@fail` function that writes to stderr
;
; This uses `write`, not `fputs` because declaring a constant corresponding to
; `stderr` is difficult to do in a cross-platform way. For example while Linux
; seems to define it as `stderr`, FreeBSD and macOS seems to define it to be
; `__stderrp`.
;
; https://github.com/austral/austral/blob/0962d2a8a5d77f7daacd7f696819520733f4897d/lib/prelude.c#L66-L80

declare i32 @write(i32, ptr, i32)
declare void @exit(i32)

; Message to write when failing. '\0A' is the hex code for the '\n' character.
@.fail-msg = private constant [5 x i8] c"fail\0A"

define void @fail() {
  call i32 @write(i32 2, ptr @.fail-msg, i32 5) ; write message to stderr
  call void @exit(i32 1)                        ; exit with an error
  unreachable
}

define i32 @main() {
  call void @fail()
  unreachable
}
