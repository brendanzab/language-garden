Usage
  $ dependent-sugar
  dependent-sugar: required COMMAND name is missing, must be either 'elab' or 'norm'.
  Usage: dependent-sugar COMMAND …
  Try 'dependent-sugar --help' for more information.
  [124]

The identity function
  $ cat >id <<< "fun (A : Type) (a : A) => a"
  $ cat id | dependent-sugar elab
  <stdin> : fun (A : Type) (a : A) -> A :=
    (fun A a => a) : fun (A : Type) (a : A) -> A
  $ cat id | dependent-sugar norm
  <stdin> : fun (A : Type) (a : A) -> A := fun A a => a

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
  $ cat bools | dependent-sugar elab
  <stdin> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  :=
    let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
    let true : Bool := fun Out true false => true;
    let false : Bool := fun Out true false => false;
    let not : fun (b : Bool) -> Bool :=
      fun b Out true false => b Out false true;
    true Bool false
  $ cat bools | dependent-sugar norm
  <stdin> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  := fun false Out true false => false

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
  $ cat options | dependent-sugar elab
  <stdin> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  :=
    let Option : fun (A : Type) -> Type :=
      fun A => fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
    let none : fun (A : Type) -> Option A := fun A Out some none => none;
    let some : fun (A : Type) (a : A) -> Option A :=
      fun A a Out some none => some a;
    some (Option Type) (some Type (Type -> Type))
  $ cat options | dependent-sugar norm
  <stdin> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  := fun Out some none => some (fun Out some none => some (Type -> Type))

Name not bound
  $ dependent-sugar elab <<< "fun (A : Type) (a : A) => foo"
  error: `foo` is not bound in the current scope
    ┌─ <stdin>:1:26
    │
  1 │ fun (A : Type) (a : A) => foo
    │                           ^^^
  [1]

Function literal body annotations (checking)
  $ dependent-sugar elab <<< "(fun A (a : A) : A => a) : fun (A : Type) (a : A) -> A"
  <stdin> : fun (A : Type) (a : A) -> A :=
    (fun A a => a) : fun (A : Type) (a : A) -> A

Mismatched function literal parameter (checking)
  $ dependent-sugar elab <<< "(fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : B) -> A"
  error: type mismatch
    expected: B
    found:    A
    ┌─ <stdin>:1:10
    │
  1 │ (fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : B) -> A
    │           ^
  [1]

Mismatched function iteral body annotation (checking)
  $ dependent-sugar elab <<< "(fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : A) -> B"
  error: type mismatch
    expected: B
    found:    A
    ┌─ <stdin>:1:19
    │
  1 │ (fun A B (a : A) : A => a) : fun (A : Type) (B : Type) (a : A) -> B
    │                    ^
  [1]

Too many parameters (checking)
  $ dependent-sugar elab <<< "(fun A B (a : A) : A => a) : fun (A : Type) (a : A) -> A"
  error: too many parameters in function literal
    ┌─ <stdin>:1:10
    │
  1 │ (fun A B (a : A) : A => a) : fun (A : Type) (a : A) -> A
    │           ^
  [1]

Function literal body annotations (inferring)
  $ dependent-sugar elab <<< "fun (A : Type) (a : A) : A => a"
  <stdin> : fun (A : Type) (a : A) -> A :=
    (fun A a => a) : fun (A : Type) (a : A) -> A

Mismatched body annotation (inferring)
  $ dependent-sugar elab <<< "fun (A : Type) (a : A) : A => A"
  error: type mismatch
    expected: A
    found:    Type
    ┌─ <stdin>:1:30
    │
  1 │ fun (A : Type) (a : A) : A => A
    │                               ^
  [1]

An example of a type error
  $ dependent-sugar elab <<EOF
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
