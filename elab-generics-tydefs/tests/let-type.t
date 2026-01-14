Initial setup
  $ alias executable=generics-tydefs

--------------------------------------------------------------------------------

Simple type definition
  $ executable elab <<< "let type Nat := Int; 4 : Nat"
  let type Nat := Int;
  4 : Int

Multiple definitions
  $ executable elab <<< "let type Nat := Int; let type Nat2 := Nat; 4 : Nat"
  let type Nat := Int;
  let type Nat2 := Nat;
  4 : Int

Single parameter
  $ executable elab <<< "let type Id A := A; 4 : Id Int"
  let type Id A := A;
  4 : Int

Multiple parameters
  $ executable elab <<< "let type Pair A B := (A, B); (3, false) : Pair Int Bool"
  let type Pair A B := (A, B);
  (3, false) : (Int, Bool)

Local type definitions
  $ executable elab <<EOF 
  >   let dup [A] (x : A) := 
  >     let type B := A;
  >     (x, x) : (B, B);
  > 
  >   dup 4
  > EOF
  let dup [A] : A -> (A, A) := fun (x : A) => let type B := A;
                                              (x, x);
  dup [Int] 4 : (Int, Int)

--------------------------------------------------------------------------------

Reused type parameter name
  $ executable elab <<< "let type Pair A A := (A, A); ()"
  error: reused type parameter name A
    ┌─ <stdin>:1:16
    │
  1 │ let type Pair A A := (A, A); ()
    │                 ^
  
  [1]
