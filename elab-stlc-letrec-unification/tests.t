Addition
  $ stlc-letrec-unification elab <<< "1 + 2"
  1 + 2 : Int

Add two function
  $ stlc-letrec-unification elab <<< "fun x => x + 2"
  fun (x : Int) => x + 2 : Int -> Int

Function application
  $ stlc-letrec-unification elab <<< "fun x f => f x * x"
  fun (x : Int) => fun (f : Int -> Int) => f x * x : Int -> (Int -> Int) -> Int

Function application
  $ stlc-letrec-unification elab <<< "let f x := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit parameter type
  $ stlc-letrec-unification elab <<< "let f (x : Int) := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Explicit return type
  $ stlc-letrec-unification elab <<< "let f (x : Int) : Int := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

Placeholder types
  $ stlc-letrec-unification elab <<< "let f (x : _) : _ := x; f 3"
  let f : Int -> Int := fun (x : Int) => x; f 3 : Int

If expressions
  $ stlc-letrec-unification elab <<< "fun x y => if x = 0 then y else 3"
  fun (x : Int) => fun (y : Int) => if x = 0 then y else 3 : Int -> Int -> Int

Recursive bindings
  $ cat >fact.txt <<EOF
  > let rec fact n :=
  >   if n = 0 then 1 else n * fact (n - 1);
  > 
  > fact 5
  > EOF

  $ cat fact.txt | stlc-letrec-unification elab
  let fact : Int -> Int :=
    #fix (fact : Int -> Int) =>
      fun (n : Int) => if n = 0 then 1 else n * fact (n - 1);
  fact 5 : Int

  $ cat fact.txt | stlc-letrec-unification norm
  120 : Int

Exposing the fixed-point combinator
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
      (fun (fact : Int -> Int) =>
         fun (n' : Int) => if n' = 0 then 1 else n' * fact (n' - 1))
      n;
  fact 5 : Int

  $ cat fix.txt | stlc-letrec-unification norm
  120 : Int

Under-applying the fixed-point combinator
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
  fun (f : (Int -> Int) -> Int -> Int) =>
    fun (x : Int) =>
      f
      ((#fix (fix : ((Int -> Int) -> Int -> Int) -> Int -> Int) =>
          fun (f' : (Int -> Int) -> Int -> Int) =>
            fun (x' : Int) => f' (fix f') x')
       f)
      x
  : ((Int -> Int) -> Int -> Int) -> Int -> Int

Naive fixed-point (this is useless in call-by-value!)
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
  fun (f : Int -> Int) =>
    f
    ((#fix (fix : (Int -> Int) -> Int) => fun (f' : Int -> Int) => f' (fix f'))
     f)
  : (Int -> Int) -> Int


Lexer Errors
------------

Unexpected character
  $ stlc-letrec-unification elab <<< "1 % 2"
  <input>:1:2: unexpected character
  [1]

Unclosed block comment
  $ stlc-letrec-unification elab <<< "/- hellooo"
  <input>:2:0: unclosed block comment
  [1]


Parse Errors
------------

Unclosed parenthesis
  $ stlc-letrec-unification elab <<< "1 + (3 "
  <input>:2:0: syntax error
  [1]



Elaboration Errors
------------------

Unbound variable
  $ stlc-letrec-unification elab <<< "let x := 1; y"
  <input>:1:12: unbound name `y`
  [1]

Mismatched definition type
  $ stlc-letrec-unification elab <<< "let x : Bool := 1; x"
  <input>:1:16: mismatched types:
    expected: Bool
    found: Int
  [1]

Mismatched argument
  $ stlc-letrec-unification elab <<< "let f x := x + 1; f f"
  <input>:1:20: mismatched types:
    expected: Int
    found: Int -> Int
  [1]

Mismatched argument
  $ stlc-letrec-unification elab <<< "let f (x : Bool) := x; f 1"
  <input>:1:25: mismatched types:
    expected: Bool
    found: Int
  [1]

Recursive let bindings
  $ stlc-letrec-unification elab <<< "let rec x := x; x : Int"
  <input>:1:0: expected function literal in recursive let binding
  [1]

Infinite type
  $ stlc-letrec-unification elab <<< "fun f => f f"
  <input>:1:11: infinite type
  [1]

Ambiguous parameter type
  $ stlc-letrec-unification elab <<< "fun x => x"
  <input>:1:4: ambiguous function parameter type
  [1]

Ambiguous return type
  $ stlc-letrec-unification elab <<< "fun f x => f x"
  <input>:1:6: ambiguous function parameter type
  <input>:1:11: ambiguous function return type
  [1]

Ambiguous placeholder
  $ stlc-letrec-unification elab <<< "fun (x : _) => x"
  <input>:1:9: unsolved placeholder
  [1]

Mismatched if expression branches
  $ stlc-letrec-unification elab <<< "fun x => if x then true else 3"
  <input>:1:29: mismatched types:
    expected: Bool
    found: Int
  [1]
