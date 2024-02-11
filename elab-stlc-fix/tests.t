Addition
  $ stlc-fix <<< "1 + 2"
  1 + 2 : Int
  3

Add two function
  $ stlc-fix <<< "fun x => x + 2"
  fun (x : Int) => x + 2 : Int -> Int
  fun (x : Int) => x + 2

Function application
  $ stlc-fix <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int
  fun (x : Int) => fun (f : Int -> Int) => f x * x

Function application
  $ stlc-fix <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

Explicit parameter type
  $ stlc-fix <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

Explicit return type
  $ stlc-fix <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

Placeholder types
  $ stlc-fix <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int
  3

If expressions
  $ stlc-fix <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3

Fix expressions: factorial
  $ stlc-fix <<< "let fact := (fun fact n => if n = 0 then 1 else n * fact (n - 1)); fix fact"
  let fact : (Int -> Int) -> Int -> Int :=
    fun (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fix fact : Int -> Int
  fix
  (fun (fact : Int -> Int) =>
     fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))


  $ stlc-fix <<< "let fact := (fun fact n => if n = 0 then 1 else n * fact (n - 1)); fix fact 5"
  let fact : (Int -> Int) -> Int -> Int :=
    fun (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fix fact 5 : Int
  120


  $ stlc-fix <<< "let fact := (fun fact n => if n = 0 then 1 else n * fact (n - 1)); fun (n: Int) => fix fact n"
  let fact : (Int -> Int) -> Int -> Int :=
    fun (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fun (n : Int) => fix fact n : Int -> Int
  fun (n : Int) =>
    fix
    (fun (fact : Int -> Int) =>
       fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))
    n

Lexer Errors
------------

Unexpected character
  $ stlc-fix <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-fix <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-fix <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-fix <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-fix <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-fix <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-fix <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Infinite type
  $ stlc-fix <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Ambiguous parameter type
  $ stlc-fix <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-fix <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-fix <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-fix <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]
