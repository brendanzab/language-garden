  $ alias executable=dependent

Usage
  $ executable
  dependent: required COMMAND name is missing, must be either 'elab' or 'norm'.
  Usage: dependent COMMAND …
  Try 'dependent --help' for more information.
  [124]

The identity function
  $ cat >id <<< "(fun A a => a) : fun (A : Type) -> A -> A"
  $ cat id | executable elab
  <stdin> : fun (A : Type) A -> A := fun A a => a
  $ cat id | executable norm
  <stdin> : fun (A : Type) A -> A := fun A a => a

Church-encoded boolean type
  $ cat >bools <<EOF
  > let Bool : Type := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false => true;
  > let false : Bool := fun Out true false => false;
  > 
  > let not : Bool -> Bool := fun b =>
  >   fun Out true false => b Out false true;
  > 
  > true Bool false
  > EOF
  $ cat bools | executable elab
  <stdin> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  :=
    let Bool : Type := fun (Out : Type) (true : Out) (false : Out) -> Out;
    let true : Bool := fun Out true false => true;
    let false : Bool := fun Out true false => false;
    let not : Bool -> Bool := fun b Out true false => b Out false true;
    true Bool false
  $ cat bools | executable norm
  <stdin> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  := fun false Out true false => false

Church-encoded option type
  $ cat >options <<EOF
  > let Option : fun (A : Type) -> Type :=
  >   fun A =>
  >     fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
  > let none : fun (A : Type) -> Option A :=
  >   fun A =>
  >     fun Out some none => none;
  > let some : fun (A : Type) -> A -> Option A :=
  >   fun A a =>
  >     fun Out some none => some a;
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
      fun A => fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
    let none : fun (A : Type) -> Option A := fun A Out some none => none;
    let some : fun (A : Type) A -> Option A := fun A a Out some none => some a;
    some (Option Type) (some Type (Type -> Type))
  $ cat options | executable norm
  <stdin> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  := fun Out some none => some (fun Out some none => some (Type -> Type))

Name not bound
  $ executable elab <<< "(fun A a => foo) : fun (A : Type) -> A -> foo"
  error: unbound name `foo`
    ┌─ <stdin>:1:42
    │
  1 │ (fun A a => foo) : fun (A : Type) -> A -> foo
    │                                           ^^^
  
  [1]

Too many parameters (checking)
  $ executable elab <<< "(fun A B a => a) : fun (A : Type) (a : A) -> A"
  error: too many parameters in function literal
    ┌─ <stdin>:1:0
    │
  1 │ (fun A B a => a) : fun (A : Type) (a : A) -> A
    │ ^^^^^^^^^^^^^^^^
  
  [1]

An example of a type error
  $ executable elab <<EOF
  > let Bool : Type := fun (Out : Type) (true : Out) (false : Out) -> Out;
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
