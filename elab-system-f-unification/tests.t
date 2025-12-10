  $ alias executable=system-f-unification

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
  $ executable elab <<< "fun (x : Int) => x + 2"
  fun (x : Int) => #int-add x 2 : Int -> Int

Function application
  $ executable elab <<EOF
  > fun (x : Int) (f : Int -> Int) =>
  >   f x * x
  > EOF
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ executable elab <<EOF
  > let f : Int -> Int :=
  >   fun x => x;
  > 
  > f 3
  > EOF
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

Check let body type
  $ executable elab <<EOF
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
  $ executable elab <<EOF
  > let f (x : Int) (y : Int) : Int :=
  >   if x = 0 then y else 3;
  > 
  > f 4
  > EOF
  let f : Int -> Int -> Int :=
    fun (x : Int) => fun (y : Int) => if #int-eq x 0 then y else 3;
  f 4 : Int -> Int

Function application
  $ executable elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => #int-mul (f x) x :
    Int -> (Int -> Int) -> Int

Function application
  $ executable elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x;
  f 3 : Int

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

Polymorphic functions
  $ executable elab <<< "(fun [a] [b] x y => x) : [a] [b] -> a -> b -> a"
  fun [a] => fun [b] => fun (x : a) => fun (y : b) => x :
    [a] -> [b] -> a -> b -> a

Polymorphic identity and constant functions
  $ executable elab <<EOF
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
  let example-2 : [b] -> b -> [a] -> a -> a :=
    always [[a] -> a -> a] (fun [$a] => id [$a]);
  false : Bool

Polymorphic identity and constant functions (inferred)
  $ executable elab <<EOF
  > let id [a] (x : a) := x;
  > let always [a] (x : a) [b] (y : b) := x;
  > 
  > let example-1 := always (id [Int]);
  > let example-2 := always (id [[a] -> a -> a]);
  > 
  > false
  > EOF
  let id : [a] -> a -> a := fun [a] => fun (x : a) => x;
  let always : [a] -> a -> [b] -> b -> a :=
    fun [a] => fun (x : a) => fun [b] => fun (y : b) => x;
  let example-1 : [b] -> b -> Int -> Int := always [Int -> Int] (id [Int]);
  let example-2 : [b] -> b -> ([a] -> a -> a) -> [a] -> a -> a :=
    always [([a] -> a -> a) -> [a] -> a -> a] (id [[a] -> a -> a]);
  false : Bool

Self application
  $ executable elab <<EOF
  > fun (x : [a] -> a -> a) =>
  >   x [[a] -> a -> a] x
  > EOF
  fun (x : [a] -> a -> a) => x [[a] -> a -> a] (fun [$a] => x [$a]) :
    ([a] -> a -> a) -> [a] -> a -> a

Self application (alternate)
  $ executable elab <<EOF
  > fun (x : [a] -> a -> a) =>
  >   x x : [a] -> a -> a
  > EOF
  fun (x : [a] -> a -> a) => fun [$a] => x [$a -> $a] (x [$a]) :
    ([a] -> a -> a) -> [a] -> a -> a

Missing type parameter insertion
  $ executable elab <<< "(fun x => x) : [a] -> a -> a"
  fun [$a] => fun (x : $a) => x : [a] -> a -> a


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
  error: unexpected type argument
    ┌─ <stdin>:1:5
    │
  1 │ true 3
    │      ^
  
  [1]

Unexpected function application
  $ executable elab <<< "true [Int]"
  error: unexpected type argument
    ┌─ <stdin>:1:5
    │
  1 │ true [Int]
    │      ^^^^^
  
  [1]

Mismatched parameter
  $ executable elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:2:11
    │
  2 │   fun (x : Int) => x;
    │            ^^^
  
  [1]

Too many parameters
  $ executable elab <<EOF
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
  $ executable elab <<EOF
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
  $ executable elab <<< "fun x => x"
  error: ambiguous function parameter type
    ┌─ <stdin>:1:4
    │
  1 │ fun x => x
    │     ^
  
  [1]

Mismatched if expression branches
  $ executable elab <<< "fun (x : Bool) => if x then true else 3"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:38
    │
  1 │ fun (x : Bool) => if x then true else 3
    │                                       ^
  
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
  error: ambiguous function argument
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

Infinite type
  $ executable elab <<< "fun f => f f"
  error: meta variable ?1 refers to itself:
    expected: ?1
       found: ?1 -> ?2
    ┌─ <stdin>:1:11
    │
  1 │ fun f => f f
    │            ^
  
  [1]

Scope escape (see https://counterexamples.org/scope-escape.html)
  $ executable elab <<EOF
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
