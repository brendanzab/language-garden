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
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ system-f-unification elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ system-f-unification elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ system-f-unification elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ system-f-unification elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ system-f-unification elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched parameter
  $ system-f-unification elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:11: mismatched types:
    expected: Int
    found: Bool
  [1]

Too many parameters
  $ system-f-unification elab <<EOF
  > let f : Bool -> Bool :=
  >   fun (x : Bool) (y : Int) => x;
  > 
  > f true
  > EOF
  <input>:2:18: unexpected parameter
  [1]

Too many type parameters
  $ system-f-unification elab <<EOF
  > let f : Bool -> Bool :=
  >   fun [a] (x : Bool) => x;
  > 
  > f true
  > EOF
  <input>:2:7: unexpected type parameter
  [1]

Ambiguous parameter type
  $ system-f-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous parameter type
  [1]

Mismatched if expression branches
  $ system-f-unification elab <<< "fun (x : Bool) => if x then true else 3"
  <input>:1:38: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched equality
  $ system-f-unification elab <<< "1 = false"
  <input>:1:0: mismatched types:
    expected: Int
    found: Bool
  [1]

Unsupported equality
  $ system-f-unification elab <<< "let f (x : Bool) := x; f = f"
  <input>:1:23: unsupported type: Bool -> Bool
  [1]

Ambiguous parameter type
  $ system-f-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous parameter type
  [1]

Ambiguous return type
  $ system-f-unification elab <<< "fun f x => f x"
  <input>:1:6: ambiguous parameter type
  <input>:1:11: ambiguous return type
  [1]

Ambiguous placeholder
  $ system-f-unification elab <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Infinite type
  $ system-f-unification elab <<< "fun f => f f"
  <input>:1:11: meta variable ?1 refers to itself:
    expected: ?1
    found: ?1 -> ?2
  [1]

Scope escape (see https://counterexamples.org/scope-escape.html)
  $ system-f-unification elab <<EOF
  > let test (id : [a] -> a -> a) : Bool :=
  >   id true;
  > 
  > fun f => test (fun [a] x => let y := f x; x)
  > EOF
  <input>:4:39: meta variable ?2 escapes its scope:
    expected: ?2
    found: a
  [1]
