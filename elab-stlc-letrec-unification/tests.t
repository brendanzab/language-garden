Boolean equality
  $ stlc-letrec-unification elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ stlc-letrec-unification elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ stlc-letrec-unification elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ stlc-letrec-unification elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ stlc-letrec-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ stlc-letrec-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ stlc-letrec-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ stlc-letrec-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Unused parameter
  $ stlc-unification elab <<< "let f (x : Int) (_ : Int) : Int := x; f 3"
  let f : Int -> Int -> Int := fun (x : Int) => fun (_ : Int) => x;
  f 3 : Int -> Int

Placeholder types
  $ stlc-letrec-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ stlc-unification elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ stlc-letrec-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
    Int -> Int -> Int


Recursive bindings: Factorial
  $ cat >fact.txt <<EOF
  > let rec fact n :=
  >   if n = 0 then 1 else n * fact (n - 1);
  > 
  > fact 5
  > EOF

  $ cat fact.txt | stlc-letrec-unification elab
  let fact : Int -> Int :=
    #fix (fact : Int -> Int) =>
      fun (n : Int) =>
        if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1));
  fact 5 : Int

  $ cat fact.txt | stlc-letrec-unification norm
  120 : Int


Recursive bindings: Factorial in terms of the fixed-point combinator
  $ cat >fix.txt <<EOF
  > let rec fix f x :=
  >   f (fix f) x;
  > 
  > let fact n :=
  >   fix (fun fact n =>
  >     if n = 0 then 1 else n * fact (n - 1)) n;
  > 
  > fact 5
  > EOF

  $ cat fix.txt | stlc-letrec-unification elab
  let fix : ((Int -> Int) -> Int -> Int) -> Int -> Int :=
    #fix (fix : ((Int -> Int) -> Int -> Int) -> Int -> Int) =>
      fun (f : (Int -> Int) -> Int -> Int) => fun (x : Int) => f (fix f) x;
  let fact : Int -> Int :=
    fun (n : Int) =>
      fix
      (fun (fact : Int -> Int) => fun (n' : Int) =>
         if #int-eq n' 0 then 1 else #int-mul n' (fact (#int-sub n' 1)))
      n;
  fact 5 : Int

  $ cat fix.txt | stlc-letrec-unification norm
  120 : Int


Recursive bindings: Under-applying the fixed-point combinator
  $ cat >fix.txt <<EOF
  > let rec fix (f : (Int -> Int) -> (Int -> Int)) (x : Int) : Int :=
  >   f (fix f) x;
  > 
  > fix
  > EOF

  $ cat fix.txt | stlc-letrec-unification elab
  let fix : ((Int -> Int) -> Int -> Int) -> Int -> Int :=
    #fix (fix : ((Int -> Int) -> Int -> Int) -> Int -> Int) =>
      fun (f : (Int -> Int) -> Int -> Int) => fun (x : Int) => f (fix f) x;
  fix : ((Int -> Int) -> Int -> Int) -> Int -> Int

  $ cat fix.txt | stlc-letrec-unification norm
  #fix (fix : ((Int -> Int) -> Int -> Int) -> Int -> Int) =>
    fun (f : (Int -> Int) -> Int -> Int) => fun (x : Int) => f (fix f) x
  : ((Int -> Int) -> Int -> Int) -> Int -> Int


Recursive bindings: Naive fixed-point (this is useless in call-by-value!)
  $ cat >fix-naive.txt <<EOF
  > let rec fix (f : Int -> Int) : Int :=
  >   f (fix f);
  > 
  > fix
  > EOF

  $ cat fix-naive.txt | stlc-letrec-unification elab
  let fix : (Int -> Int) -> Int :=
    #fix (fix : (Int -> Int) -> Int) => fun (f : Int -> Int) => f (fix f);
  fix : (Int -> Int) -> Int

  $ cat fix-naive.txt | stlc-letrec-unification norm
  #fix (fix : (Int -> Int) -> Int) => fun (f : Int -> Int) => f (fix f) :
    (Int -> Int) -> Int


Recursive bindings: Ackermann function
  $ cat >ack.txt <<EOF
  > let rec ack m n :=
  >   if m = 0 then
  >     n + 1
  >   else if n = 0 then
  >     ack (m - 1) 1
  >   else
  >     ack (m - 1) (ack m (n - 1));
  > 
  > ack 1 0
  > EOF

  $ cat ack.txt | stlc-letrec-unification elab
  let ack : Int -> Int -> Int :=
    #fix (ack : Int -> Int -> Int) =>
      fun (m : Int) => fun (n : Int) =>
        if #int-eq m 0 then
          #int-add n 1
        else
          if #int-eq n 0 then
            ack (#int-sub m 1) 1
          else
            ack (#int-sub m 1) (ack m (#int-sub n 1));
  ack 1 0 : Int

  $ cat ack.txt | stlc-letrec-unification norm
  2 : Int


Recursive bindings: Ackermann function (partially applied)
  $ cat >ack-partial-app.txt <<EOF
  > let rec ack m n :=
  >   if m = 0 then
  >     n + 1
  >   else if n = 0 then
  >     ack (m - 1) 1
  >   else
  >     ack (m - 1) (ack m (n - 1));
  > 
  > ack 0
  > EOF

  $ cat ack-partial-app.txt | stlc-letrec-unification elab
  let ack : Int -> Int -> Int :=
    #fix (ack : Int -> Int -> Int) =>
      fun (m : Int) => fun (n : Int) =>
        if #int-eq m 0 then
          #int-add n 1
        else
          if #int-eq n 0 then
            ack (#int-sub m 1) 1
          else
            ack (#int-sub m 1) (ack m (#int-sub n 1));
  ack 0 : Int -> Int

  $ cat ack-partial-app.txt | stlc-letrec-unification norm
  fun (n : Int) => #int-add n 1 : Int -> Int


Recursive bindings: Count-down (partially applied)
  $ cat >count-down.txt <<EOF
  > let rec count-down x n :=
  >   if n = 0 then x else count-down x (n - 1);
  > 
  > count-down true
  > EOF

  $ cat count-down.txt | stlc-letrec-unification elab
  let count-down : Bool -> Int -> Bool :=
    #fix (count-down : Bool -> Int -> Bool) =>
      fun (x : Bool) => fun (n : Int) =>
        if #int-eq n 0 then x else count-down x (#int-sub n 1);
  count-down true : Int -> Bool

  $ cat count-down.txt | stlc-letrec-unification norm
  fun (n : Int) =>
    if #int-eq n 0 then
      true
    else
      (#fix (count-down : Bool -> Int -> Bool) =>
         fun (x : Bool) => fun (n' : Int) =>
           if #int-eq n' 0 then x else count-down x (#int-sub n' 1))
      true (#int-sub n 1)
  : Int -> Bool


Recursive bindings: Even/odd (partially applied)
  $ cat >even-odd-partial-app.txt <<EOF
  > let rec even-odd b n :=
  >   if b then (if n = 0 then true else even-odd false (n - 1))
  >        else (if n = 0 then false else even-odd true (n - 1));
  > 
  > even-odd true
  > EOF

  $ cat even-odd-partial-app.txt | stlc-letrec-unification elab
  let even-odd : Bool -> Int -> Bool :=
    #fix (even-odd : Bool -> Int -> Bool) =>
      fun (b : Bool) => fun (n : Int) =>
        if b then
          (if #int-eq n 0 then true else even-odd false (#int-sub n 1))
        else
          if #int-eq n 0 then false else even-odd true (#int-sub n 1);
  even-odd true : Int -> Bool

  $ cat even-odd-partial-app.txt | stlc-letrec-unification norm
  fun (n : Int) =>
    if #int-eq n 0 then
      true
    else
      (#fix (even-odd : Bool -> Int -> Bool) =>
         fun (b : Bool) => fun (n' : Int) =>
           if b then
             (if #int-eq n' 0 then true else even-odd false (#int-sub n' 1))
           else
             if #int-eq n' 0 then false else even-odd true (#int-sub n' 1))
      false (#int-sub n 1)
  : Int -> Bool


Mutually recursive bindings: Even/odd
  $ cat >even-odd.txt <<EOF
  > let rec is-even n :=
  >       if n = 0 then true else is-odd (n - 1);
  >     rec is-odd n :=
  >       if n = 0 then false else is-even (n - 1);
  > 
  > is-even 6
  > EOF

  $ cat even-odd.txt | stlc-letrec-unification elab
  let $mutual-0 : (Int -> Bool, Int -> Bool) :=
    #fix ($mutual-0 : (Int -> Bool, Int -> Bool)) =>
      (fun (n : Int) =>
         if #int-eq n 0 then true else $mutual-0.1 (#int-sub n 1),
      fun (n : Int) =>
        if #int-eq n 0 then false else $mutual-0.0 (#int-sub n 1));
  $mutual-0.0 6 : Bool

  $ cat even-odd.txt | stlc-letrec-unification norm
  true : Bool


Mutually recursive bindings: Even/odd (partially applied)
  $ cat >even-odd.txt <<EOF
  > let rec is-even n :=
  >       if n = 0 then true else is-odd (n - 1);
  >     rec is-odd n :=
  >       if n = 0 then false else is-even (n - 1);
  > 
  > is-even
  > EOF

  $ cat even-odd.txt | stlc-letrec-unification elab
  let $mutual-0 : (Int -> Bool, Int -> Bool) :=
    #fix ($mutual-0 : (Int -> Bool, Int -> Bool)) =>
      (fun (n : Int) =>
         if #int-eq n 0 then true else $mutual-0.1 (#int-sub n 1),
      fun (n : Int) =>
        if #int-eq n 0 then false else $mutual-0.0 (#int-sub n 1));
  $mutual-0.0 : Int -> Bool

  $ cat even-odd.txt | stlc-letrec-unification norm
  fun (n : Int) =>
    if #int-eq n 0 then
      true
    else
      (#fix ($mutual-0 : (Int -> Bool, Int -> Bool)) =>
         (fun (n' : Int) =>
            if #int-eq n' 0 then true else $mutual-0.1 (#int-sub n' 1),
         fun (n' : Int) =>
           if #int-eq n' 0 then false else $mutual-0.0 (#int-sub n' 1))).1
      (#int-sub n 1)
  : Int -> Bool


Lexer Errors
------------

Unexpected character
  $ stlc-letrec-unification elab <<< "1 % 2"
  error: unexpected character
    ┌─ <stdin>:1:2
    │
  1 │ 1 % 2
    │   ^
  [1]

Unclosed block comment
  $ stlc-letrec-unification elab <<< "/- hellooo"
  error: unclosed block comment
    ┌─ <stdin>:1:10
    │
  1 │ /- hellooo
    │           ^
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-letrec-unification elab <<< "1 + (3 "
  error: syntax error
    ┌─ <stdin>:1:7
    │
  1 │ 1 + (3 
    │        ^
  [1]


Elaboration Errors
------------------

Unbound variable
  $ stlc-letrec-unification elab <<< "let x := 1; y"
  error: unbound name `y`
    ┌─ <stdin>:1:12
    │
  1 │ let x := 1; y
    │             ^
  [1]

Mismatched definition type
  $ stlc-letrec-unification elab <<< "let x : Bool := 1; x"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
  [1]

Mismatched argument
  $ stlc-letrec-unification elab <<< "let f x := x + 1; f f"
  error: mismatched types:
    expected: Int
    found: Int -> Int
    ┌─ <stdin>:1:20
    │
  1 │ let f x := x + 1; f f
    │                     ^
  [1]

Mismatched argument
  $ stlc-letrec-unification elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
  [1]

Recursive let bindings
  $ stlc-letrec-unification elab <<< "let rec x := x; x : Int"
  error: expected function literal in recursive let binding
    ┌─ <stdin>:1:8
    │
  1 │ let rec x := x; x : Int
    │         ^
  [1]

Infinite type
  $ stlc-letrec-unification elab <<< "fun f => f f"
  error: infinite type
    ┌─ <stdin>:1:11
    │
  1 │ fun f => f f
    │            ^
  [1]

Unexpected parameter
  $ stlc-unification elab <<< "(fun x y => x) : Int -> Int"
  error: unexpected parameter
    ┌─ <stdin>:1:7
    │
  1 │ (fun x y => x) : Int -> Int
    │        ^
  [1]

Ambiguous parameter type
  $ stlc-letrec-unification elab <<< "fun x => x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  [1]

Ambiguous return type
  $ stlc-letrec-unification elab <<< "fun f x => f x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:6
    │
  1 │ fun f x => f x
    │       ^
  error: ambiguous function return type
    ┌─ <stdin>:1:11
    │
  1 │ fun f x => f x
    │            ^
  [1]

Ambiguous placeholder
  $ stlc-letrec-unification elab <<< "fun (x : _) => x"
  error: unsolved placeholder
    ┌─ <stdin>:1:9
    │
  1 │ fun (x : _) => x
    │          ^
  [1]

Mismatched if expression branches
  $ stlc-letrec-unification elab <<< "fun x => if x then true else 3"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:29
    │
  1 │ fun x => if x then true else 3
    │                              ^
  [1]

Mismatched equality
  $ stlc-letrec-unification elab <<< "1 = false"
  error: mismatched types:
    expected: Int
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ 1 = false
    │ ^^^^^^^^^
  [1]

Unsupported equality
  $ stlc-letrec-unification elab <<< "let f (x : Bool) := x; f = f"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
  [1]
