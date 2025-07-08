  $ alias executable=dependent-sugar

Usage
  $ executable
  dependent-sugar: required COMMAND name is missing, must be either 'elab' or 'norm'.
  Usage: dependent-sugar COMMAND …
  Try 'dependent-sugar --help' for more information.
  [124]

The identity function
  $ cat >id <<< "fun (A : Type) (a : A) => a"
  $ cat id | executable elab
  <stdin> : fun (A : Type) (a : A) -> A := fun (A : Type) (a : A) => a
  $ cat id | executable norm
  <stdin> : fun (A : Type) (a : A) -> A := fun (A : Type) (a : A) => a

Church-encoded boolean type
  $ cat >bools <<EOF
  > let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false => true;
  > let false : Bool := fun Out true false => false;
  > 
  > let not (b : Bool) : Bool := 
  >   fun (Out : Type) (true : Out) (false : Out) => b Out false true;
  > 
  > true Bool false
  > EOF
  $ cat bools | executable elab
  <stdin> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  :=
    let Bool : Type := fun (Out : Type) (true : Out) (false : Out) -> Out;
    let true : fun (Out : Type) (true : Out) (false : Out) -> Out :=
      fun (Out : Type) (true : Out) (false : Out) => true;
    let false : fun (Out : Type) (true : Out) (false : Out) -> Out :=
      fun (Out : Type) (true : Out) (false : Out) => false;
    let not :
          fun (b : fun (Out : Type) (true : Out) (false : Out) -> Out)
              (Out : Type) (true : Out) (false : Out) -> Out
    :=
      fun (b : Bool) (Out : Type) (true : Out) (false : Out) =>
        b Out false true;
    true Bool false
  $ cat bools | executable norm
  <stdin> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  :=
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
    (Out : Type) (true : Out) (false : Out) =>
      false

Church-encoded option type
  $ cat >options <<EOF
  > let Option (A : Type) : Type :=
  >   fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
  > let none (A : Type) : Option A :=
  >   fun Out some none => none;
  > let some (A : Type) (a : A) : Option A :=
  >   fun Out some none => some a;
  > 
  > some (Option Type) (some Type (Type -> Type))
  > EOF
  $ cat options | executable elab
  <stdin> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  :=
    let Option : fun (A : Type) -> Type :=
      fun (A : Type) => fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
    let none :
          fun (A : Type) (Out : Type) (some : A -> Out) (none : Out) -> Out
    := fun (A : Type) (Out : Type) (some : A -> Out) (none : Out) => none;
    let some :
          fun (A : Type) (a : A) (Out : Type) (some : A -> Out) (none : Out)
              -> Out
    :=
      fun (A : Type) (a : A) (Out : Type) (some : A -> Out) (none : Out) =>
        some a;
    some (Option Type) (some Type (Type -> Type))
  $ cat options | executable norm
  <stdin> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  :=
    fun (Out : Type)
    (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) -> Out)
    (none : Out) =>
      some
        (fun (Out : Type) (some : Type -> Out) (none : Out) =>
           some (Type -> Type))

Name not bound
  $ executable elab <<< "fun (A : Type) (a : A) => foo"
  error: unbound name `foo`
    ┌─ <stdin>:1:26
    │
  1 │ fun (A : Type) (a : A) => foo
    │                           ^^^
  
  [1]

Function literal body annotations (checking)
  $ executable elab <<< "(fun A (a : A) : A => a) : fun (A : Type) (a : A) -> A"
  <stdin> : fun (A : Type) (a : A) -> A := fun (A : Type) (a : A) => a

Mismatched function literal parameter (checking)
  $ executable elab <<< "(fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : B) -> A"
  error: type mismatch
    expected: B
    found:    A
    ┌─ <stdin>:1:10
    │
  1 │ (fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : B) -> A
    │           ^
  
  [1]

Mismatched function iteral body annotation (checking)
  $ executable elab <<< "(fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : A) -> B"
  error: type mismatch
    expected: B
    found:    A
    ┌─ <stdin>:1:19
    │
  1 │ (fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : A) -> B
    │                    ^
  
  [1]

Too many parameters (checking)
  $ executable elab <<< "(fun A B (a : A) : A => a) : fun (A : Type) (a : A) -> A"
  error: too many parameters in function literal
    ┌─ <stdin>:1:10
    │
  1 │ (fun A B (a : A) : A => a) : fun (A : Type) (a : A) -> A
    │           ^
  
  [1]

Function literal body annotations (inferring)
  $ executable elab <<< "fun (A : Type) (a : A) : A => a"
  <stdin> : fun (A : Type) (a : A) -> A := fun (A : Type) (a : A) => a

Mismatched body annotation (inferring)
  $ executable elab <<< "fun (A : Type) (a : A) : A => A"
  error: type mismatch
    expected: A
    found:    Type
    ┌─ <stdin>:1:30
    │
  1 │ fun (A : Type) (a : A) : A => A
    │                               ^
  
  [1]

An example of a type error
  $ executable elab <<EOF
  > let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false => true;
  > let false : Bool := fun Out true false => false;
  > 
  > true Type : Bool
  > EOF
  error: type mismatch
    expected: fun (Out : Type) (true : Out) (false : Out) -> Out
    found:    fun (true : Type) (false : Type) -> Type
    ┌─ <stdin>:5:0
    │
  5 │ true Type : Bool
    │ ^^^^^^^^^
  
  [1]
