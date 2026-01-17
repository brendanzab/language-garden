Initial setup
  $ alias executable=generics-lettype

--------------------------------------------------------------------------------

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

  $ cat fact.txt | executable eval
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
  Usage: generics-lettype [--help] COMMAND …
  generics-lettype: unknown command n. Must be either elab or eval
  [124]

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

  $ cat even-odd.txt | executable eval
  true : Bool

--------------------------------------------------------------------------------

Placeholder name in mutually recursive binding
  $ executable elab <<< "let rec { _ x := x; f x := x }; f 1 : Int"
  error: placeholder in recursive binding
    ┌─ <stdin>:1:10
    │
  1 │ let rec { _ x := x; f x := x }; f 1 : Int
    │           ^
  
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
