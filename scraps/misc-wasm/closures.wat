;; A demonstration of implementing anonymous functions in Wasm using closures.
;; This is the kind of code we could generate after closure conversion.
;;
;; Closures are made up of two parts: a pointer to some some code, and the
;; an environment of bindings that the closure captures:
;;
;; ```
;;     ┌─────┬─────┐
;;     │  ●  │  ●  │  closure  (dynamically allocated)
;;     └──│──┴──│──┘
;;        │     │     ┌─────┬ - - ┬─────┐
;;        │     └────▶︎│  x₁          xₙ │  env  (dynamically allocated)
;;        │           └─────┴ - - ┴─────┘
;;        ▼
;;     ┌───────────────────────┐
;;     │ (env, args...) -> ret │  code
;;     └───────────────────────┘
;; ```
;;
;; Resources:
;;
;; - Matt Might. Closure conversion: How to compile lambda.
;;   https://matt.might.net/articles/closure-conversion/
;; - Wasm GC Proposal: Closures.
;;   https://github.com/WebAssembly/spec/blob/main/proposals/gc/Overview.md#closures

;; TODO: Remove double indirection, as demonstrated in the GC proposal:
;; https://github.com/WebAssembly/gc/blob/main/proposals/gc/Overview.md#closures
;; Unfortunately my previous attempts at this have been unsuccessful.

(module
  ;; Base definitions

  (tag $fail (export "fail"))

  ;; fun () -> I32
  (type $code-to-i32 (func (param $env (ref any)) (result i32)))
  (type $clos-to-i32 (struct (field $code (ref $code-to-i32)) (field $env (ref any))))

  ;; fun (I32) -> I32
  (type $code-i32-to-i32 (func (param $env (ref any)) (param i32) (result i32)))
  (type $clos-i32-to-i32 (struct (field $code (ref $code-i32-to-i32)) (field $env (ref any))))

  ;; fun (I32, I32) -> I32
  (type $code-i32-i32-to-i32 (func (param $env (ref any)) (param i32) (param i32) (result i32)))
  (type $clos-i32-i32-to-i32 (struct (field $code (ref $code-i32-i32-to-i32)) (field $env (ref any))))


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
  ;; -- From https://github.com/WebAssembly/gc/blob/main/proposals/gc/Overview.md#closures
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

  (type $f.g.env (struct (field $x i32)))

  (elem declare func $f.g.code)

  (func $f.g.code (param $env (ref any)) (result i32)
    (local.get $env)
    (ref.cast (ref $f.g.env))
    (struct.get $f.g.env $x)
  )

  (func $f (export "f") (param $x i32) (result (ref $clos-to-i32))
    (struct.new $clos-to-i32
      (ref.func $f.g.code)
      (struct.new $f.g.env (local.get $x))
    )
  )

  ;; Non-capturing closures
  ;;
  ;; ```
  ;; fun h() : fun () -> I32 :=
  ;;   fun () -> 3;
  ;; ```

  (type $h.res.env (struct))

  (elem declare func $h.res.code)

  (func $h.res.code (param $env (ref any)) (result i32)
    (i32.const 3)
  )

  (func $h (export "h") (result (ref $clos-to-i32))
    (struct.new $clos-to-i32
      (ref.func $h.res.code)
      (struct.new $h.res.env)
    )
  )

  ;; Closures with arguments
  ;;
  ;; ```
  ;; fun curried-add(x : I32) : fun (I32) -> I32 :=
  ;;   fun (y) -> x + y;
  ;; ```

  (type $curried-add.res.env (struct (field $x i32)))

  (elem declare func $curried-add.res.code)

  (func $curried-add.res.code (param $env (ref any)) (param $y i32) (result i32)
    (i32.add
      (struct.get $curried-add.res.env $x
        (ref.cast (ref $curried-add.res.env)
          (local.get $env)))
      (local.get $y)
    )
  )

  (func $curried-add (export "curried-add") (param $x i32) (result (ref $clos-i32-to-i32))
    (struct.new $clos-i32-to-i32
      (ref.func $curried-add.res.code)
      (struct.new $curried-add.res.env (local.get $x))
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

  (type $local-capture.inner.env (struct (field $x i32) (field $a i32)))

  (elem declare func $local-capture.inner.code)

  (func $local-capture.inner.code (param $env (ref any)) (param $y i32) (result i32)
    (i32.add
      (i32.add
        (local.get $y)
        (struct.get $local-capture.inner.env $a
          (ref.cast (ref $local-capture.inner.env)
            (local.get $env))))
      (struct.get $local-capture.inner.env $x
        (ref.cast (ref $local-capture.inner.env)
          (local.get $env)))
    )
  )

  (func $local-capture (export "local-capture") (param $x i32) (result (ref $clos-i32-to-i32))
    (local $a i32)
    (local $inner (ref $clos-i32-to-i32))
    (local.set $a (i32.add (local.get $x) (i32.const 1)))
    (local.set $inner
      (struct.new $clos-i32-to-i32
        (ref.func $local-capture.inner.code)
        (struct.new $local-capture.inner.env (local.get $x) (local.get $a))
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
          (struct.get $clos-to-i32 $env (local.get $clos1))
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
          (struct.get $clos-to-i32 $env (local.get $clos2))
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
          (struct.get $clos-i32-to-i32 $env (local.get $clos3))
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
          (struct.get $clos-i32-to-i32 $env (local.get $clos3))
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
