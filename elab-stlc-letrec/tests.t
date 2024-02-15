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

Fix expressions: factorial
  $ stlc-letrec <<< "let fact := (fun fact n => if n = 0 then 1 else n * fact (n - 1)); fix fact"
  let fact : (Int -> Int) -> Int -> Int :=
    fun (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fix fact : Int -> Int
  fix
  (fun (fact : Int -> Int) =>
     fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))


  $ stlc-letrec <<< "let fact := (fun fact n => if n = 0 then 1 else n * fact (n - 1)); fix fact 5"
  let fact : (Int -> Int) -> Int -> Int :=
    fun (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fix fact 5 : Int
  120


  $ stlc-letrec <<< "let fact := (fun fact n => if n = 0 then 1 else n * fact (n - 1)); fun (n: Int) => fix fact n"
  let fact : (Int -> Int) -> Int -> Int :=
    fun (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fun (n : Int) => fix fact n : Int -> Int
  fun (n : Int) =>
    fix
    (fun (fact : Int -> Int) =>
       fun (n : Int) => if n = 0 then 1 else n * fact (n - 1))
    n

Fix expressions: tail recursive factorial
  $ stlc-letrec <<< "let fact := (fun fact acc n => if n = 0 then acc else fact (acc * n) (n - 1)); fix fact 1"
  let fact : (Int -> Int -> Int) -> Int -> Int -> Int :=
    fun (fact : Int -> Int -> Int) =>
      fun (acc : Int) =>
        fun (n : Int) => if n = 0 then acc else fact (acc * n) (n - 1);
  fix fact 1 : Int -> Int
  fun (n : Int) => if n = 0 then 1 else
    fix
    (fun (fact : Int -> Int -> Int) =>
       fun (acc : Int) =>
         fun (n : Int) => if n = 0 then acc else fact (acc * n) (n - 1))
    (1 * n) (n - 1)

Fix expressions: even/odd
  $ stlc-letrec <<< "let evenodd := (fun evenodd b => if b then (fun n => if n = 0 then true else evenodd false (n - 1)) else (fun n => if n = 0 then false else evenodd true (n - 1))); fix evenodd true"
  let evenodd : (Bool -> Int -> Bool) -> Bool -> Int -> Bool :=
    fun (evenodd : Bool -> Int -> Bool) =>
      fun (b : Bool) => if b then
        (fun (n : Int) => if n = 0 then true else evenodd false (n - 1)) else
        (fun (n : Int) => if n = 0 then false else evenodd true (n - 1));
  fix evenodd true : Int -> Bool
  fun (n : Int) => if n = 0 then true else
    fix
    (fun (evenodd : Bool -> Int -> Bool) =>
       fun (b : Bool) => if b then
         (fun (n : Int) => if n = 0 then true else evenodd false (n - 1)) else
         (fun (n : Int) => if n = 0 then false else evenodd true (n - 1)))
    false (n - 1)


  $ stlc-letrec <<< "let evenodd := (fun evenodd b => if b then (fun n => if n = 0 then true else evenodd false (n - 1)) else (fun n => if n = 0 then false else evenodd true (n - 1))); fix evenodd true 0"
  let evenodd : (Bool -> Int -> Bool) -> Bool -> Int -> Bool :=
    fun (evenodd : Bool -> Int -> Bool) =>
      fun (b : Bool) => if b then
        (fun (n : Int) => if n = 0 then true else evenodd false (n - 1)) else
        (fun (n : Int) => if n = 0 then false else evenodd true (n - 1));
  fix evenodd true 0 : Bool
  true

  $ stlc-letrec <<< "let evenodd := (fun evenodd b => if b then (fun n => if n = 0 then true else evenodd false (n - 1)) else (fun n => if n = 0 then false else evenodd true (n - 1))); fix evenodd true 1"
  let evenodd : (Bool -> Int -> Bool) -> Bool -> Int -> Bool :=
    fun (evenodd : Bool -> Int -> Bool) =>
      fun (b : Bool) => if b then
        (fun (n : Int) => if n = 0 then true else evenodd false (n - 1)) else
        (fun (n : Int) => if n = 0 then false else evenodd true (n - 1));
  fix evenodd true 1 : Bool
  false

  $ stlc-letrec <<< "let evenodd := (fun evenodd b => if b then (fun n => if n = 0 then true else evenodd false (n - 1)) else (fun n => if n = 0 then false else evenodd true (n - 1))); fix evenodd true 2"
  let evenodd : (Bool -> Int -> Bool) -> Bool -> Int -> Bool :=
    fun (evenodd : Bool -> Int -> Bool) =>
      fun (b : Bool) => if b then
        (fun (n : Int) => if n = 0 then true else evenodd false (n - 1)) else
        (fun (n : Int) => if n = 0 then false else evenodd true (n - 1));
  fix evenodd true 2 : Bool
  true

Fix expressions: ackermann
  $ stlc-letrec <<< "let ack ack m n := if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)); fix ack"
  let ack : (Int -> Int -> Int) -> Int -> Int -> Int :=
    fun (ack : Int -> Int -> Int) =>
      fun (m : Int) =>
        fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
          else ack (m - 1) (ack m (n - 1));
  fix ack : Int -> Int -> Int
  fix
  (fun (ack : Int -> Int -> Int) =>
     fun (m : Int) =>
       fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
         else ack (m - 1) (ack m (n - 1)))


  $ stlc-letrec <<< "let ack ack m n := if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)); fix ack 0 1"
  let ack : (Int -> Int -> Int) -> Int -> Int -> Int :=
    fun (ack : Int -> Int -> Int) =>
      fun (m : Int) =>
        fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
          else ack (m - 1) (ack m (n - 1));
  fix ack 0 1 : Int
  2


  $ stlc-letrec <<< "let ack ack m n := if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)); fix ack 1 0"
  let ack : (Int -> Int -> Int) -> Int -> Int -> Int :=
    fun (ack : Int -> Int -> Int) =>
      fun (m : Int) =>
        fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
          else ack (m - 1) (ack m (n - 1));
  fix ack 1 0 : Int
  2


  $ stlc-letrec <<< "let ack ack m n := if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)); fix ack 0"
  let ack : (Int -> Int -> Int) -> Int -> Int -> Int :=
    fun (ack : Int -> Int -> Int) =>
      fun (m : Int) =>
        fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
          else ack (m - 1) (ack m (n - 1));
  fix ack 0 : Int -> Int
  fun (n : Int) => n + 1


  $ stlc-letrec <<< "let ack ack m n := if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)); fix ack 1"
  let ack : (Int -> Int -> Int) -> Int -> Int -> Int :=
    fun (ack : Int -> Int -> Int) =>
      fun (m : Int) =>
        fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
          else ack (m - 1) (ack m (n - 1));
  fix ack 1 : Int -> Int
  fun (n : Int) => if n = 0 then
    fix
    (fun (ack : Int -> Int -> Int) =>
       fun (m : Int) =>
         fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
           else ack (m - 1) (ack m (n - 1)))
    0 1 else
    fix
    (fun (ack : Int -> Int -> Int) =>
       fun (m : Int) =>
         fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
           else ack (m - 1) (ack m (n - 1)))
    0
    (fix
     (fun (ack : Int -> Int -> Int) =>
        fun (m : Int) =>
          fun (n : Int) => if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1
            else ack (m - 1) (ack m (n - 1)))
     1 (n - 1))


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
