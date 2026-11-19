;; A demonstration of implementing anonymous functions in Wasm using closures.
;; This is the kind of code we could generate after closure conversion.
;;
;; Closures are made up of two parts: a pointer to some some code, and the
;; an environment of bindings that the closure captures. Unlike
;;  `closures-nested-env.wat`, the environment is stored inline with the code
;; pointer in a single dynamic allocation:
;;
;; ```
;;     ┌─────┬─────┬ - - ┬─────┐
;;     │  ●  │  x₁          xₙ │  closure  (dynamically allocated)
;;     └──│──┴─────┴ - - ┴─────┘
;;        │  └────── env ──────┘
;;        │
;;        ▼
;;     ┌────────────────────────┐
;;     │ (clos, args...) -> ret │  code
;;     └────────────────────────┘
;; ```
;;
;; Note that the full closure (including the code pointer) is passed to the code
;; function. This is  unconventional, but seems necessary due to us not being
;; able to take a pointer partway through a struct in Wasm (unlike in LLVM for
;; example). This approach was adapted from the example given the Wasm GC
;; proposal linked below.
;;
;; Resources:
;;
;; - Matt Might. Closure conversion: How to compile lambda.
;;   https://matt.might.net/articles/closure-conversion/
;; - Wasm GC Proposal: Closures.
;;   https://github.com/WebAssembly/spec/blob/main/proposals/gc/Overview.md#closures

(module
  ;; Base definitions

  (tag $fail (export "fail"))

  ;; fun () -> I32
  (type $code-to-i32 (func (param $clos (ref any)) (result i32)))
  (type $clos-to-i32 (sub (struct (field $code (ref $code-to-i32)))))
  ;;                  ^^^ allow future subtypes to be defined

  ;; fun (I32) -> I32
  (type $code-i32-to-i32 (func (param $clos (ref any)) (param i32) (result i32)))
  (type $clos-i32-to-i32 (sub (struct (field $code (ref $code-i32-to-i32)))))

  ;; fun (I32, I32) -> I32
  (type $code-i32-i32-to-i32 (func (param $clos (ref any)) (param i32) (param i32) (result i32)))
  (type $clos-i32-i32-to-i32 (sub (struct (field $code (ref $code-i32-i32-to-i32)))))


  ;; Main Code
  ;;
  ;; ```
  ;; fun assert(b : Bool) : Unit :=
  ;;   if b then () else fail();
  ;;
  ;; fun f(x : I32) : fun () -> I32 :=
  ;;   let g = fun() -> x;
  ;;   g;
  ;;
  ;; fun h() : fun () -> I32 :=
  ;;   fun() -> 3;
  ;;
  ;; fun curried-add(x : I32) : fun (I32) -> I32 :=
  ;;   fun(y) -> x + y;
  ;;
  ;; -- https://github.com/WebAssembly/spec/blob/main/proposals/gc/Overview.md#closures
  ;; fun local-capture(x : I32) : fun (I32) -> I32 {
  ;;   let a := x + 1;
  ;;   let inner := fun (y : I32) -> y + a + x;
  ;;   inner;
  ;;
  ;; fun add-i32(x : I32, y : I32) : I32 := x + y;
  ;; fun add-i32-indirect() : fun (I32, I32) -> I32 := add-i32;
  ;;
  ;; fun main() :=
  ;;   let () := assert (f(4)() = 4);
  ;;   let () := assert (f()() = 3);
  ;;   let () := assert (curried-add(3)(5) = 8);
  ;;   let () := assert (local-capture(1)(2) = 5);
  ;;   let () := assert (add-i32-indirect()(3, 4) = 7);
  ;;   ()
  ;; ```

  (func $assert (param $b i32)
    (if (local.get $b) (then) (else (throw $fail)))
  )

  ;; Closures with captures
  ;;
  ;; ```
  ;; fun f(x : I32) : fun () -> I32 :=
  ;;   let g = fun () -> x;
  ;;   g;
  ;; ```

  (type $f.g.clos (sub $clos-to-i32 (struct
    ;;                 ^^^^^^^^^^^^ defined as a subtype of the appropriate closure type
    (field $code (ref $code-to-i32))
    (field $x i32)
  )))

  (elem declare func $f.g.code)

  (func $f.g.code (param $clos (ref any)) (result i32)
    (struct.get $f.g.clos $x
      (ref.cast (ref $f.g.clos) ;; cast to the appropriate environment type
        (local.get $clos)))
  )

  (func $f (export "f") (param $x i32) (result (ref $clos-to-i32))
    (struct.new $f.g.clos
      (ref.func $f.g.code)
      (local.get $x)
    )
  )

  ;; Non-capturing closures
  ;;
  ;; ```
  ;; fun h() : fun () -> I32 :=
  ;;   fun () -> 3;
  ;; ```

  (type $h.res.clos (sub $clos-to-i32 (struct
    (field $code (ref $code-to-i32))
  )))

  (elem declare func $h.res.code)

  (func $h.res.code (param $clos (ref any)) (result i32)
    (i32.const 3)
  )

  (func $h (export "h") (result (ref $clos-to-i32))
    (struct.new $h.res.clos
      (ref.func $h.res.code)
    )
  )

  ;; Closures with arguments
  ;;
  ;; ```
  ;; fun curried-add(x : I32) : fun (I32) -> I32 :=
  ;;   fun (y) -> x + y;
  ;; ```

  (type $curried-add.res.clos (sub $clos-i32-to-i32 (struct
    (field $code (ref $code-i32-to-i32))
    (field $x i32)
  )))

  (elem declare func $curried-add.res.code)

  (func $curried-add.res.code (param $clos (ref any)) (param $y i32) (result i32)
    (i32.add
      (struct.get $curried-add.res.clos $x
        (ref.cast (ref $curried-add.res.clos)
          (local.get $clos)))
      (local.get $y)
    )
  )

  (func $curried-add (export "curried-add") (param $x i32) (result (ref $clos-i32-to-i32))
    (struct.new $curried-add.res.clos
      (ref.func $curried-add.res.code)
      (local.get $x)
    )
  )

  ;; Closures capturing local definitions
  ;;
  ;; ```
  ;; fun local-capture(x : I32) : fun (I32) -> I32 :=
  ;;   let a := x + 1;
  ;;   let inner := fun (y : I32) -> y + a + x;
  ;;   inner;
  ;; ```

  (type $local-capture.inner.clos (sub $clos-i32-to-i32 (struct
    (field $code (ref $code-i32-to-i32))
    (field $x i32)
    (field $a i32)
  )))

  (elem declare func $local-capture.inner.code)

  (func $local-capture.inner.code (param $clos (ref any)) (param $y i32) (result i32)
    (i32.add
      (i32.add
        (local.get $y)
        (struct.get $local-capture.inner.clos $a
          (ref.cast (ref $local-capture.inner.clos)
            (local.get $clos))))
      (struct.get $local-capture.inner.clos $x
        (ref.cast (ref $local-capture.inner.clos)
          (local.get $clos)))
    )
  )

  (func $local-capture (export "local-capture") (param $x i32) (result (ref $clos-i32-to-i32))
    (local $a i32)
    (local $inner (ref $clos-i32-to-i32))
    (local.set $a (i32.add (local.get $x) (i32.const 1)))
    (local.set $inner
      (struct.new $local-capture.inner.clos
        (ref.func $local-capture.inner.code)
        (local.get $x)
        (local.get $a)
      )
    )
    (local.get $inner)
  )

  ;; References to top-level functions will be wrapped in closures (eta-expanded)
  ;; to ensure that all function values follow the same calling convention.
  ;;
  ;; ```
  ;; fun add-i32(x : I32, y : I32) : I32 := x + y;
  ;; fun add-i32-indirect() : fun (I32, I32) -> I32 := add-i32;
  ;; ```

  ;; TODO

  (func $main (export "main")
    (local $clos1 (ref $clos-to-i32))
    (local $clos2 (ref $clos-to-i32))
    (local $clos3 (ref $clos-i32-to-i32))

    ;; let () := assert (f(4)() = 4);

    (local.set $clos1 (call $f (i32.const 4)))
    (call $assert
      (i32.eq
        (call_ref $code-to-i32
          (local.get $clos1)
          (struct.get $clos-to-i32 $code (local.get $clos1))
        )
        (i32.const 4)
      )
    )

    ;; let () := assert (h()() = 3);

    (local.set $clos2 (call $h))
    (call $assert
      (i32.eq
        (call_ref $code-to-i32
          (local.get $clos2)
          (struct.get $clos-to-i32 $code (local.get $clos2))
        )
        (i32.const 3)
      )
    )

    ;; let () := assert (curried-add(3)(5) = 8);

    (local.set $clos3 (call $curried-add (i32.const 3)))
    (call $assert
      (i32.eq
        (call_ref $code-i32-to-i32
          (local.get $clos3)
          (i32.const 5)
          (struct.get $clos-i32-to-i32 $code (local.get $clos3))
        )
        (i32.const 8)
      )
    )

    ;; let () := assert (local-capture(1)(2) = 5);

    (local.set $clos3 (call $local-capture (i32.const 1)))
    (call $assert
      (i32.eq
        (call_ref $code-i32-to-i32
          (local.get $clos3)
          (i32.const 2)
          (struct.get $clos-i32-to-i32 $code (local.get $clos3))
        )
        (i32.const 5)
      )
    )

    ;; let () := assert (add-i32-indirect()(3, 4) = 7);

    ;; TODO
  )
)
