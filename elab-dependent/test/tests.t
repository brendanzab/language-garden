Usage
  $ dependent
  dependent: required COMMAND name is missing, must be either 'elab' or 'norm'.
  Usage: dependent COMMAND …
  Try 'dependent --help' for more information.
  [124]

The identity function
  $ cat >id <<< "(fun A a => a) : fun (A : Type) -> A -> A"
  $ cat id | dependent elab
  <input> : fun (A : Type) A -> A := (fun A a := a) : fun (A : Type) A -> A
  $ cat id | dependent norm
  <input> : fun (A : Type) A -> A := fun A a := a

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
  $ cat bools | dependent elab
  <input> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  :=
    let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
    let true := fun Out true false := true;
    let false := fun Out true false := false;
    let not := fun b Out true false := b Out false true; true Bool false
  $ cat bools | dependent norm
  <input> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  := fun false Out true false := false

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
  $ cat options | dependent elab
  <input> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  :=
    let Option :=
      fun A := fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
    let none := fun A Out some none := none;
    let some := fun A a Out some none := some a;
    some (Option Type) (some Type (Type -> Type))
  $ cat options | dependent norm
  <input> :
    fun (Out : Type)
        (some : (fun (Out : Type) (some : Type -> Out) (none : Out) -> Out) ->
          Out)
        (none : Out) -> Out
  := fun Out some none := some (fun Out some none := some (Type -> Type))

Name not bound
  $ dependent elab <<< "(fun A a => foo) : fun (A : Type) -> A -> foo"
  error: `foo` is not bound in the current scope
  [1]

Too many parameters (checking)
  $ dependent elab <<< "(fun A B a => a) : fun (A : Type) (a : A) -> A"
  error: too many parameters in function literal
  [1]

An example of a type error
  $ dependent elab <<EOF
  > let Bool : Type := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false => true;
  > let false : Bool := fun Out true false => false;
  > 
  > true Type : Bool
  > EOF
  error: type mismatch
    expected: fun (Out : Type) (true : Out) (false : Out) -> Out
    found:    fun (true : Type) (false : Type) -> Type
  [1]
