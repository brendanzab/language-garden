Addition
  $ stlc-letrec <<< "1 + 2"
  1 + 2 : Int
  3

Add two function
  $ stlc-letrec <<< "fun x => x + 2"
  fun (x : Int) => x + 2 : Int -> Int
  fun (x : Int) => x + 2

Function application
  $ stlc-letrec <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
  fun (x : Int) => fun (f : Int -> Int) => f x * x

Function application
  $ stlc-letrec <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

Explicit parameter type
  $ stlc-letrec <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

Explicit return type
  $ stlc-letrec <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

Placeholder types
  $ stlc-letrec <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

If expressions
  $ stlc-letrec <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3

Record literals
  $ stlc-letrec <<< "{x := 1; y := false}"
  { x := 1; y := false } : { x : Int; y : Bool }
  { x := 1; y := false }

Record projection
  $ stlc-letrec <<< "{x := 1; y := false}.x"
  { x := 1; y := false }.x : Int
  1

Fix1: factorial
  $ cat >fact.txt <<EOF
  > fix1 (fun fact n => if n = 0 then 1 else n * fact (n - 1))
  > EOF
  $ cat fact.txt | stlc-letrec
  fix
  (fun (fact : Int -> Int) =>
     fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))
  : Int -> Int
  fix
  (fun (fact : Int -> Int) =>
     fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))


  $ echo '5' >> fact.txt
  $ cat fact.txt | stlc-letrec
  fix
  (fun (fact : Int -> Int) =>
     fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))
  5 : Int
  120

Fix2: even/odd
  $ cat >even-odd.txt <<EOF
  > (fix2 (fun group => {
  >   f1 := fun n => if n = 0 then true  else group.f2 (n - 1);
  >   f2 := fun n => if n = 0 then false else group.f1 (n - 1)
  > }))
  > EOF
  $ cat even-odd.txt | stlc-letrec
  fix2
  (fun (group : { f1 : Int -> Bool; f2 : Int -> Bool }) =>
     { f1 := fun (n : Int) => if n = 0 then true else group.f2 (n - 1); f2 := 
     fun (n : Int) => if n = 0 then false else group.f1 (n - 1) })
  : { f1 : Int -> Bool; f2 : Int -> Bool }
  fix2
  (fun (group : { f1 : Int -> Bool; f2 : Int -> Bool }) =>
     { f1 := fun (n : Int) => if n = 0 then true else group.f2 (n - 1); f2 := 
     fun (n : Int) => if n = 0 then false else group.f1 (n - 1) })

  $ echo '.f1' >> even-odd.txt
  $ cat even-odd.txt | stlc-letrec
  (fix2
   (fun (group : { f1 : Int -> Bool; f2 : Int -> Bool }) =>
      { f1 := fun (n : Int) => if n = 0 then true else group.f2 (n - 1); f2 := 
      fun (n : Int) => if n = 0 then false else group.f1 (n - 1) })).f1
  : Int -> Bool
  (fix2
   (fun (group : { f1 : Int -> Bool; f2 : Int -> Bool }) =>
      { f1 := fun (n : Int) => if n = 0 then true else group.f2 (n - 1); f2 := 
      fun (n : Int) => if n = 0 then false else group.f1 (n - 1) })).f1

  $ echo '3' >> even-odd.txt
  $ cat even-odd.txt | stlc-letrec
  (fix2
   (fun (group : { f1 : Int -> Bool; f2 : Int -> Bool }) =>
      { f1 := fun (n : Int) => if n = 0 then true else group.f2 (n - 1); f2 := 
      fun (n : Int) => if n = 0 then false else group.f1 (n - 1) })).f1
  3 : Bool
  false

Lexer Errors
------------

Unexpected character
  $ stlc-letrec <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-letrec <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-letrec <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-letrec <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-letrec <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-letrec <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-letrec <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Infinite type
  $ stlc-letrec <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Ambiguous parameter type
  $ stlc-letrec <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-letrec <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-letrec <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-letrec <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]
