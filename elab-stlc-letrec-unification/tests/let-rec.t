Initial setup
  $ alias executable=stlc-letrec-unification

--------------------------------------------------------------------------------

Factorial
  $ cat >fact.txt <<EOF
  > let rec fact n :=
  >   if n = 0 then 1 else n * fact (n - 1);
  > 
  > fact 5
  > EOF

  $ cat fact.txt | executable elab
  let fact : Int -> Int :=
    #fix (fact : Int -> Int) =>
      fun (n : Int) =>
        if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1));
  fact 5 : Int

  $ cat fact.txt | executable norm
  120 : Int

Factorial (partially applied)
  $ cat >fix.txt <<EOF
  > let rec fix (f : (Int -> Int) -> (Int -> Int)) (x : Int) : Int :=
  >   f (fix f) x;
  > 
  > fix
  > EOF

  $ cat fix.txt | executable elab
  let fix : ((Int -> Int) -> Int -> Int) -> Int -> Int :=
    #fix (fix : ((Int -> Int) -> Int -> Int) -> Int -> Int) =>
      fun (f : (Int -> Int) -> Int -> Int) => fun (x : Int) => f (fix f) x;
  fix : ((Int -> Int) -> Int -> Int) -> Int -> Int

  $ cat fix.txt | executable norm
  #fix (fix : ((Int -> Int) -> Int -> Int) -> Int -> Int) =>
    fun (f : (Int -> Int) -> Int -> Int) => fun (x : Int) => f (fix f) x
  : ((Int -> Int) -> Int -> Int) -> Int -> Int

Factorial in terms of the fixed-point combinator
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

  $ cat fix.txt | executable elab
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

  $ cat fix.txt | executable norm
  120 : Int

Naive fixed-point (this is useless in call-by-value!)
  $ cat >fix-naive.txt <<EOF
  > let rec fix (f : Int -> Int) : Int :=
  >   f (fix f);
  > 
  > fix
  > EOF

  $ cat fix-naive.txt | executable elab
  let fix : (Int -> Int) -> Int :=
    #fix (fix : (Int -> Int) -> Int) => fun (f : Int -> Int) => f (fix f);
  fix : (Int -> Int) -> Int

  $ cat fix-naive.txt | executable norm
  #fix (fix : (Int -> Int) -> Int) => fun (f : Int -> Int) => f (fix f) :
    (Int -> Int) -> Int

Ackermann function
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

  $ cat ack.txt | executable elab
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

  $ cat ack.txt | executable norm
  2 : Int

Ackermann function (partially applied)
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

  $ cat ack-partial-app.txt | executable elab
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

  $ cat ack-partial-app.txt | executable norm
  fun (n : Int) => #int-add n 1 : Int -> Int

Count-down (partially applied)
  $ cat >count-down.txt <<EOF
  > let rec count-down x n :=
  >   if n = 0 then x else count-down x (n - 1);
  > 
  > count-down true
  > EOF

  $ cat count-down.txt | executable elab
  let count-down : Bool -> Int -> Bool :=
    #fix (count-down : Bool -> Int -> Bool) =>
      fun (x : Bool) => fun (n : Int) =>
        if #int-eq n 0 then x else count-down x (#int-sub n 1);
  count-down true : Int -> Bool

  $ cat count-down.txt | executable norm
  fun (n : Int) =>
    if #int-eq n 0 then
      true
    else
      (#fix (count-down : Bool -> Int -> Bool) =>
         fun (x : Bool) => fun (n' : Int) =>
           if #int-eq n' 0 then x else count-down x (#int-sub n' 1))
        true
        (#int-sub n 1)
  : Int -> Bool

Even/odd
  $ cat >even-odd.txt <<EOF
  > let rec {
  >   is-even n := if n = 0 then true else is-odd (n - 1);
  >   is-odd n := if n = 0 then false else is-even (n - 1);
  > };
  > 
  > is-even 6
  > EOF

  $ cat even-odd.txt | executable elab
  let $mutual-0 : { is-even : Int -> Bool; is-odd : Int -> Bool } :=
    #fix ($mutual-0 : { is-even : Int -> Bool; is-odd : Int -> Bool }) =>
      {
        is-even :=
          fun (n : Int) =>
            if #int-eq n 0 then true else $mutual-0.is-odd (#int-sub n 1);
        is-odd :=
          fun (n : Int) =>
            if #int-eq n 0 then false else $mutual-0.is-even (#int-sub n 1);
      };
  $mutual-0.is-even 6 : Bool

  $ cat even-odd.txt | executable norm
  true : Bool

Even/odd (partially applied)
  $ cat >even-odd.txt <<EOF
  > let rec {
  >   is-even n := if n = 0 then true else is-odd (n - 1);
  >   is-odd n := if n = 0 then false else is-even (n - 1);
  > };
  > 
  > is-even
  > EOF

  $ cat even-odd.txt | executable elab
  let $mutual-0 : { is-even : Int -> Bool; is-odd : Int -> Bool } :=
    #fix ($mutual-0 : { is-even : Int -> Bool; is-odd : Int -> Bool }) =>
      {
        is-even :=
          fun (n : Int) =>
            if #int-eq n 0 then true else $mutual-0.is-odd (#int-sub n 1);
        is-odd :=
          fun (n : Int) =>
            if #int-eq n 0 then false else $mutual-0.is-even (#int-sub n 1);
      };
  $mutual-0.is-even : Int -> Bool

  $ cat even-odd.txt | executable norm
  fun (n : Int) =>
    if #int-eq n 0 then
      true
    else
      (#fix ($mutual-0 : { is-even : Int -> Bool; is-odd : Int -> Bool }) =>
         {
           is-even :=
             fun (n' : Int) =>
               if #int-eq n' 0 then true else $mutual-0.is-odd (#int-sub n' 1);
           is-odd :=
             fun (n' : Int) =>
               if #int-eq n' 0 then
                 false
               else
                 $mutual-0.is-even (#int-sub n' 1);
         })
        .is-odd
        (#int-sub n 1)
  : Int -> Bool

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

Ambiguous mutually recursive bindings
  $ executable elab <<EOF
  > let rec {
  >   foo n := bar n;
  >   bar n := foo n;
  > };
  > 
  > foo
  > EOF
  error: ambiguous definition type
    ┌─ <stdin>:2:2
    │
  2 │   foo n := bar n;
    │   ^^^
  
  error: ambiguous parameter type
    ┌─ <stdin>:3:6
    │
  3 │   bar n := foo n;
    │       ^
  
  [1]
