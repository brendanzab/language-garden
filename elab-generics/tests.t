  $ alias executable=generics

Boolean equality
  $ executable elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ executable elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ executable elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ executable elab <<< "fun x => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ executable elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ executable elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ executable elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ executable elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Unused parameter
  $ executable elab <<< "let f (x : Int) (_ : Int) : Int := x; f 3"
  let f : Int -> Int -> Int := fun (x : Int) => fun (_ : Int) => x;
  f 3 : Int -> Int

Placeholder types
  $ executable elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ executable elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ executable elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
    Int -> Int -> Int

Polymorphism examples
---------------------

Identity function
  $ executable elab <<< "let id [A] (x : A) : A := x; id 42"
  let id [A] : A -> A := fun (x : A) => x;
  id [Int] 42 : Int

Identity function (explicit type application)
  $ executable elab <<< "let id [A] (x : A) : A := x; id [Int] 42"
  let id [A] : A -> A := fun (x : A) => x;
  id [Int] 42 : Int

Identity function (placeholder type application)
  $ executable elab <<< "let id [A] (x : A) : A := x; id [_] 42"
  let id [A] : A -> A := fun (x : A) => x;
  id [Int] 42 : Int

Identity and constant functions
  $ executable elab <<EOF
  > let id [A] (x : A) : A := x;
  > let const [A, B] (x : A) (y : B) : A := x;
  > 
  > const 5 (id true)
  > EOF
  let id [A] : A -> A := fun (x : A) => x;
  let const [A, B] : A -> B -> A := fun (x : A) => fun (y : B) => x;
  const [Int, Bool] 5 (id [Bool] true) : Int

Identity and constant functions (alternate syntax)
  $ executable elab <<EOF
  > let {
  >   id [A] (x : A) : A := x;
  >   const [A, B] (x : A) (y : B) : A := x;
  > };
  > 
  > const 5 (id true)
  > EOF
  let id [A] : A -> A := fun (x : A) => x;
  let const [A, B] : A -> B -> A := fun (x : A) => fun (y : B) => x;
  const [Int, Bool] 5 (id [Bool] true) : Int

Locally polymorphic definitions
  $ executable elab <<EOF
  > -- False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html
  > let kite [A, B] : A -> B -> B :=
  >   fun x =>
  >     let id [A] (x : A) : A := x;
  >     id; 
  > 
  > kite 5 true
  > EOF
  let kite [A, B] : A -> B -> B :=
    fun (x : A) => let id [A] : A -> A := fun (x : A) => x;
                   id [B];
  kite [Int, Bool] 5 true : Bool

Factorial
  $ cat >fact.txt <<EOF
  > let rec fact n :=
  >   if n = 0 then 1 else n * fact (n - 1);
  > 
  > fact 5
  > EOF

  $ cat fact.txt | executable elab
  let rec {
    fact : Int -> Int :=
      fun (n : Int) =>
        if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1));
  };
  fact 5 : Int

  $ cat fact.txt | executable norm
  120 : Int

Factorial in terms of the fixed-point combinator
  $ cat >fix.txt <<EOF
  > let rec fix [A, B] (f : (A -> B) -> A -> B) (x : A) : B :=
  >   f (fix f) x;
  > 
  > let fact n :=
  >   fix (fun fact n =>
  >     if n = 0 then 1 else n * fact (n - 1)) n;
  > 
  > fact 5
  > EOF

  $ cat fix.txt | executable elab
  let rec {
    fix [A, B] : ((A -> B) -> A -> B) -> A -> B :=
      fun (f : (A -> B) -> A -> B) => fun (x : A) => f (fix [A, B] f) x;
  };
  let fact : Int -> Int :=
    fun (n : Int) =>
      fix
        [Int, Int]
        (fun (fact : Int -> Int) => fun (n : Int) =>
           if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1)))
        n;
  fact 5 : Int
  $ cat fix.txt | executable n
  120 : Int

Mutually recursive bindings: Even/odd
  $ cat >even-odd.txt <<EOF
  > let rec {
  >   is-even n := if n = 0 then true else is-odd (n - 1);
  >   is-odd n := if n = 0 then false else is-even (n - 1);
  > };
  > 
  > is-even 6
  > EOF

  $ cat even-odd.txt | executable elab
  let rec {
    is-even : Int -> Bool :=
      fun (n : Int) => if #int-eq n 0 then true else is-odd (#int-sub n 1);
    is-odd : Int -> Bool :=
      fun (n : Int) => if #int-eq n 0 then false else is-even (#int-sub n 1);
  };
  is-even 6 : Bool

  $ cat even-odd.txt | executable norm
  true : Bool

Lexer Errors
------------

Unexpected character
  $ executable elab <<< "1 % 2"
  error: unexpected character
    ┌─ <stdin>:1:2
    │
  1 │ 1 % 2
    │   ^
  
  [1]

Unclosed block comment
  $ executable elab <<< "/- hellooo"
  error: unclosed block comment
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ executable elab <<< "1 + (3 "
  error: syntax error
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  
  [1]



Elaboration Errors
------------------

Unbound variable
  $ executable elab <<< "let x := 1; y"
  error: unbound name `y`
    ┌─ <stdin>:1:12
    │
  1 │ let x := 1; y
    │             ^
  
  [1]

Mismatched definition type
  $ executable elab <<< "let x : Bool := 1; x"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
  
  [1]

Mismatched argument
  $ executable elab <<< "let f x := x + 1; f f"
  error: mismatched types:
    expected: Int
       found: Int -> Int
    ┌─ <stdin>:1:20
    │
  1 │ let f x := x + 1; f f
    │                     ^
  
  [1]

Mismatched argument
  $ executable elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
  
  [1]

Unexpected function application
  $ executable elab <<< "true 3"
  error: unexpected argument
    ┌─ <stdin>:1:5
    │
  1 │ true 3
    │      ^
  
  [1]

Infinite type
  $ executable elab <<< "fun f => f f"
  error: infinite type
    ┌─ <stdin>:1:11
    │
  1 │ fun f => f f
    │            ^
  
  [1]

Unexpected parameter
  $ executable elab <<< "(fun x y => x) : Int -> Int"
  error: unexpected parameter
    ┌─ <stdin>:1:7
    │
  1 │ (fun x y => x) : Int -> Int
    │        ^
  
  [1]

Ambiguous parameter type
  $ executable elab <<< "fun x => x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  
  [1]

Ambiguous return type
  $ executable elab <<< "fun f x => f x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:11
    │
  1 │ fun f x => f x
    │            ^
  
  error: ambiguous function return type
    ┌─ <stdin>:1:11
    │
  1 │ fun f x => f x
    │            ^
  
  [1]

Ambiguous placeholder
  $ executable elab <<< "fun (x : _) => x"
  error: ambiguous placeholder
    ┌─ <stdin>:1:9
    │
  1 │ fun (x : _) => x
    │          ^
  
  [1]

Mismatched if expression branches
  $ executable elab <<< "fun x => if x then true else 3"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:29
    │
  1 │ fun x => if x then true else 3
    │                              ^
  
  [1]

Mismatched equality
  $ executable elab <<< "1 = false"
  error: mismatched types:
    expected: Int
       found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ 1 = false
    │ ^^^^^^^^^
  
  [1]

Unsupported equality
  $ executable elab <<< "let f (x : Bool) := x; f = f"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
  
  [1]

Identity function (invalid type application)
  $ executable elab <<< "let id [A] (x : A) : A := x; id [Bool] 42"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:39
    │
  1 │ let id [A] (x : A) : A := x; id [Bool] 42
    │                                        ^^
  
  [1]

Identity function (reused type parameter)
  $ executable elab <<< "let id [A, A] (x : A) : A := x; id 42"
  error: reused type parameter name A
    ┌─ <stdin>:1:11
    │
  1 │ let id [A, A] (x : A) : A := x; id 42
    │            ^
  
  [1]

Identity function (ambiguous type argument)
  $ executable elab <<< "let id [A] (x : A) : A := x; id"
  error: ambiguous type argument
    ┌─ <stdin>:1:29
    │
  1 │ let id [A] (x : A) : A := x; id
    │                              ^^
  
  [1]

Reused name in mutually recursive binding
  $ executable elab <<< "let rec { f x := x; f x := x }; f 1 : Int"
  error: reused name `f` in recursive binding
    ┌─ <stdin>:1:20
    │
  1 │ let rec { f x := x; f x := x }; f 1 : Int
    │                     ^
  
  [1]

Ignored definition in mutually recursive binding
  $ executable elab <<< "let rec { f x := x; _ x := x }; f 1 : Int"
  error: placeholder in recursive binding
    ┌─ <stdin>:1:20
    │
  1 │ let rec { f x := x; _ x := x }; f 1 : Int
    │                     ^
  
  [1]

Vicious circle prevention
  $ executable elab <<< "let rec x := x; x : Int"
  error: definitions must be functions in recursive let bindings
    ┌─ <stdin>:1:8
    │
  1 │ let rec x := x; x : Int
    │         ^
  
  [1]

Mismatched recursive call
  $ executable elab <<EOF
  > let rec {
  >   is-even (n : Int) := if n = 0 then true else is-odd false;
  >   is-odd (n : Int) := if n = 0 then false else is-even (n - 1);
  > };
  > 
  > is-even
  > EOF
  error: mismatched types:
    expected: Int
       found: Bool
    ┌─ <stdin>:2:54
    │
  2 │   is-even (n : Int) := if n = 0 then true else is-odd false;
    │                                                       ^^^^^
  
  [1]

Infinite type
  $ executable elab <<< "fun f => f f"
  error: infinite type
    ┌─ <stdin>:1:11
    │
  1 │ fun f => f f
    │            ^
  
  [1]
