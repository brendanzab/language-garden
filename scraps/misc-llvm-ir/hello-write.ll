; Hello world, using the `write` system call
;
; - https://linux.die.net/man/2/write
; - https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/write.2.html

declare i32 @write(i32, ptr, i32)

; The message to write to stdout. '\0A' is the hex code for the '\n' character.
@.greeting = private constant [14 x i8] c"Hello, world!\0A"

define i32 @main() {
  call i32 @write(i32 1, ptr @.greeting, i32 14) ; write the message to stdout
  ret i32 0
}
