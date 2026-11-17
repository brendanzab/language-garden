;; Example of compiling join points to web assembly blocks. This could be used
;; when compiling ANF to web assembly.
;;
;; ```
;; fun test(x : I32) : I32 :=
;;   join j0 (p : I32) : I32 := p + x;
;;   if x = 0 then
;;     jump j0 1
;;   else
;;     jump j0 2;
;; ```
;;
;; - Norman Ramsey. 2022. Beyond Relooper: recursive translation of unstructured
;;   control flow to structured control flow (functional pearl).
;;   <https://doi.org/10.1145/3547621>

(module
  (func
    $test
    (export "test")
    (param $x0 i32)
    (result i32)
    (return
      (i32.add
        (block $j0 (result i32)
        ;;     ^^^ join point
          (if (result i32)
            (i32.eq (local.get $x0) (i32.const 0))
            (then (br $j0 (i32.const 1)))
            ;;     ^^^^^^ jump to join point
            (else (br $j0 (i32.const 2)))
            ;;     ^^^^^^ jump to join point
          )
        )
        (local.get $x0)
      )
    )
  )

  (func $assert (param $b i32)
    (if (local.get $b) (then) (else unreachable))
  )

  (func $main (export "main")
    (call $assert (i32.eq (call $test (i32.const 0)) (i32.const 1)))
    (call $assert (i32.eq (call $test (i32.const 1)) (i32.const 3)))
  )
)
