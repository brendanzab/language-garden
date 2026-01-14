Initial setup
  $ alias executable=generics

--------------------------------------------------------------------------------

Identity function
  $ executable elab <<< "let id [A] (x : A) : A := x; id 42"
  let id [A] : A -> A := fun (x : A) => x;
  id [Int] 42 : Int

Identity function (explicit type application)
  $ executable elab <<< "let id [A] (x : A) : A := x; id [Int] 42"
  let id [A] : A -> A := fun (x : A) => x;
  id [Int] 42 : Int

Identity function (placeholder type application)
  $ executable elab <<< "let id [A] (x : A) : A := x; id [_] 42"
  let id [A] : A -> A := fun (x : A) => x;
  id [Int] 42 : Int

Identity and constant functions
  $ executable elab <<EOF
  > let id [A] (x : A) : A := x;
  > let const [A, B] (x : A) (y : B) : A := x;
  > 
  > const 5 (id true)
  > EOF
  let id [A] : A -> A := fun (x : A) => x;
  let const [A, B] : A -> B -> A := fun (x : A) => fun (y : B) => x;
  const [Int, Bool] 5 (id [Bool] true) : Int

Identity and constant functions (alternate syntax)
  $ executable elab <<EOF
  > let {
  >   id [A] (x : A) : A := x;
  >   const [A, B] (x : A) (y : B) : A := x;
  > };
  > 
  > const 5 (id true)
  > EOF
  let id [A] : A -> A := fun (x : A) => x;
  let const [A, B] : A -> B -> A := fun (x : A) => fun (y : B) => x;
  const [Int, Bool] 5 (id [Bool] true) : Int

Locally polymorphic definitions
  $ executable elab <<EOF
  > -- False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html
  > let kite [A, B] : A -> B -> B :=
  >   fun x =>
  >     let id [A] (x : A) : A := x;
  >     id; 
  > 
  > kite 5 true
  > EOF
  let kite [A, B] : A -> B -> B :=
    fun (x : A) => let id [A] : A -> A := fun (x : A) => x;
                   id [B];
  kite [Int, Bool] 5 true : Bool

--------------------------------------------------------------------------------

Invalid type application
  $ executable elab <<< "let id [A] (x : A) : A := x; id [Bool] 42"
  error: mismatched types:
    expected: Bool
       found: Int
    ┌─ <stdin>:1:39
    │
  1 │ let id [A] (x : A) : A := x; id [Bool] 42
    │                                        ^^
  
  [1]

Reused type parameter
  $ executable elab <<< "let id [A, A] (x : A) : A := x; id 42"
  error: reused type parameter name A
    ┌─ <stdin>:1:11
    │
  1 │ let id [A, A] (x : A) : A := x; id 42
    │            ^
  
  [1]

Ambiguous type argument
  $ executable elab <<< "let id [A] (x : A) : A := x; id"
  error: ambiguous type argument
    ┌─ <stdin>:1:29
    │
  1 │ let id [A] (x : A) : A := x; id
    │                              ^^
  
  [1]
