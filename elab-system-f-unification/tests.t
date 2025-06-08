Boolean equality
  $ system-f-unification elab <<< "true = false"
  #bool-eq true false : Bool

Integer equality
  $ system-f-unification elab <<< "1 = 2"
  #int-eq 1 2 : Bool

Integer Addition
  $ system-f-unification elab <<< "1 + 2"
  #int-add 1 2 : Int

Add two function
  $ system-f-unification elab <<< "fun (x : Int) => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ system-f-unification elab <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ system-f-unification elab <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit parameter type
  $ system-f-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Explicit return type
  $ system-f-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Check let body type
  $ system-f-unification elab <<EOF
  > let f (x : Int) : Int -> Int :=
  >   let id (x : Int) : Int := x;
  >   let incr (x : Int) : Int := x + 1;
  > 
  >   if x = 0 then id else incr;
  > 
  > f 4 3
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) =>
      let id : Int -> Int := fun (x : Int) => x;
      let incr : Int -> Int := fun (x : Int) => #int-add x 1;
      if #int-eq x 0 then id else incr;
  f 4 3 : Int

If expressions
  $ system-f-unification elab <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3;
  f 4 : Int -> Int

Function application
  $ system-f-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ system-f-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder types
  $ system-f-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

Placeholder return type
  $ system-f-unification elab <<< "let f : Int -> _ := fun x y => x; f 3 true"
  let f : Int -> Bool -> Int := fun (x : Int) => fun (y : Bool) => x;
  f 3 true : Int

If expressions
  $ system-f-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3 :
    Int -> Int -> Int

Polymorphic functions
  $ system-f-unification elab <<< "(fun [a] [b] x y => x) : [a] [b] -> a -> b -> a"
  fun [a] => fun [b] => fun (x : a) => fun (y : b) => x :
    [a] -> [b] -> a -> b -> a

Polymorphic identity and constant functions
  $ system-f-unification elab <<EOF
  > let id [a] (x : a) := x;
  > let always [a] (x : a) [b] (y : b) := x;
  > 
  > let example-1 := always [Int -> Int] (id [Int]);
  > let example-2 := always [[a] -> a -> a] id;
  > 
  > false
  > EOF
  let id : [a] -> a -> a := fun [a] => fun (x : a) => x;
  let always : [a] -> a -> [b] -> b -> a :=
    fun [a] => fun (x : a) => fun [b] => fun (y : b) => x;
  let example-1 : [b] -> b -> Int -> Int := always [Int -> Int] (id [Int]);
  let example-2 : [b] -> b -> [a] -> a -> a := always [[a] -> a -> a] id;
  false : Bool

Polymorphic identity and constant functions (inferred)
  $ system-f-unification elab <<EOF
  > let id [a] (x : a) := x;
  > let always [a] (x : a) [b] (y : b) := x;
  > 
  > let example-1 := always (id [Int]);
  > let example-2 := always id;
  > 
  > false
  > EOF
  let id : [a] -> a -> a := fun [a] => fun (x : a) => x;
  let always : [a] -> a -> [b] -> b -> a :=
    fun [a] => fun (x : a) => fun [b] => fun (y : b) => x;
  let example-1 : [b] -> b -> Int -> Int := always [Int -> Int] (id [Int]);
  let example-2 : [b] -> b -> [a] -> a -> a := always [[a] -> a -> a] id;
  false : Bool

Self application
  $ system-f-unification elab <<EOF
  > fun (x : [a] -> a -> a) =>
  >   x [[a] -> a -> a] x
  > EOF
  fun (x : [a] -> a -> a) => x [[a] -> a -> a] x :
    ([a] -> a -> a) -> [a] -> a -> a

Self application (inferred)
  $ system-f-unification elab <<EOF
  > fun (x : [a] -> a -> a) =>
  >   x x
  > EOF
  fun (x : [a] -> a -> a) => x [[a] -> a -> a] x :
    ([a] -> a -> a) -> [a] -> a -> a

Missing type parameter insertion
  $ system-f-bidirectional elab <<< "(fun x => x) : [a] -> a -> a"
  fun [$a] => fun (x : $a) => x : [a] -> a -> a


Lexer Errors
------------

Unexpected character
  $ system-f-unification elab <<< "1 % 2"
  error: unexpected character
    ┌─ <stdin>:1:2
    │
  1 │ 1 % 2
    │   ^
  [1]

Unclosed block comment
  $ system-f-unification elab <<< "/- hellooo"
  error: unclosed block comment
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ system-f-unification elab <<< "1 + (3 "
  error: syntax error
    ┌─ <stdin>:2:0
    │
  2 │ 
    │ ^
  [1]



Elaboration Errors
------------------

Unbound variable
  $ system-f-unification elab <<< "let x := 1; y"
  error: unbound name `y`
    ┌─ <stdin>:1:12
    │
  1 │ let x := 1; y
    │             ^
  [1]

Mismatched definition type
  $ system-f-unification elab <<< "let x : Bool := 1; x"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:16
    │
  1 │ let x : Bool := 1; x
    │                 ^
  [1]

Mismatched argument
  $ system-f-unification elab <<< "let f (x : Bool) := x; f 1"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:25
    │
  1 │ let f (x : Bool) := x; f 1
    │                          ^
  [1]

Unexpected function application
  $ system-f-unification elab <<< "true 3"
  error: mismatched types:
    expected: function
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ true 3
    │ ^^^^
  [1]

Unexpected function application
  $ system-f-unification elab <<< "true [Int]"
  error: mismatched types:
    expected: forall
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ true [Int]
    │ ^^^^
  [1]

Mismatched parameter
  $ system-f-unification elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  error: mismatched types:
    expected: Int
    found: Bool
    ┌─ <stdin>:2:11
    │
  2 │   fun (x : Int) => x;
    │            ^^^
  [1]

Too many parameters
  $ system-f-unification elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Bool) (y : Int) => x;
  > 
  > f true
  > EOF
  error: unexpected parameter
    ┌─ <stdin>:2:18
    │
  2 │   fun (x : Bool) (y : Int) => x;
    │                   ^
  [1]

Too many type parameters
  $ system-f-unification elab <<EOF
  > let f : Bool -> Bool :=
  >   fun [a] (x : Bool) => x;
  > 
  > f true
  > EOF
  error: unexpected type parameter
    ┌─ <stdin>:2:7
    │
  2 │   fun [a] (x : Bool) => x;
    │        ^
  [1]

Ambiguous parameter type
  $ system-f-unification elab <<< "fun x => x"
  error: ambiguous parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  [1]

Mismatched if expression branches
  $ system-f-unification elab <<< "fun (x : Bool) => if x then true else 3"
  error: mismatched types:
    expected: Bool
    found: Int
    ┌─ <stdin>:1:38
    │
  1 │ fun (x : Bool) => if x then true else 3
    │                                       ^
  [1]

Mismatched equality
  $ system-f-unification elab <<< "1 = false"
  error: mismatched types:
    expected: Int
    found: Bool
    ┌─ <stdin>:1:0
    │
  1 │ 1 = false
    │ ^^^^^^^^^
  [1]

Unsupported equality
  $ system-f-unification elab <<< "let f (x : Bool) := x; f = f"
  error: unsupported type: Bool -> Bool
    ┌─ <stdin>:1:23
    │
  1 │ let f (x : Bool) := x; f = f
    │                        ^^^^^
  [1]

Ambiguous parameter type
  $ system-f-unification elab <<< "fun x => x"
  error: ambiguous parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  [1]

Ambiguous return type
  $ system-f-unification elab <<< "fun f x => f x"
  error: ambiguous parameter type
    ┌─ <stdin>:1:6
    │
  1 │ fun f x => f x
    │       ^
  error: ambiguous return type
    ┌─ <stdin>:1:11
    │
  1 │ fun f x => f x
    │            ^
  [1]

Ambiguous placeholder
  $ system-f-unification elab <<< "fun (x : _) => x"
  error: unsolved placeholder
    ┌─ <stdin>:1:9
    │
  1 │ fun (x : _) => x
    │          ^
  [1]

Infinite type
  $ system-f-unification elab <<< "fun f => f f"
  error: meta variable ?1 refers to itself:
    expected: ?1
    found: ?1 -> ?2
    ┌─ <stdin>:1:11
    │
  1 │ fun f => f f
    │            ^
  [1]

Scope escape (see https://counterexamples.org/scope-escape.html)
  $ system-f-unification elab <<EOF
  > let test (id : [a] -> a -> a) : Bool :=
  >   id true;
  > 
  > fun f => test (fun x => let y := f x; x)
  > EOF
  error: type variable $a escapes the scope of meta variable ?2:
    expected: ?2
    found: $a
    ┌─ <stdin>:4:35
    │
  4 │ fun f => test (fun x => let y := f x; x)
    │                                    ^
  [1]
