; Example of returning a non-zero exit code

declare void @exit(i32)

define void @exit-error() {
  call void @exit(i32 1)
  unreachable
}

define i32 @main() {
  call void @exit-error()
  ret i32 0
}
