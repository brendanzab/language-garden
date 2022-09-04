The identity function
  $ cat >id <<< "fun (A : Type) (a : A) := a"
  $ cat id | elab-dependent elab
  <input> : fun (A : Type) (a : A) -> A :=
    (fun A a := a) : fun (A : Type) (a : A) -> A
  $ cat id | elab-dependent norm
  <input> : fun (A : Type) (a : A) -> A := fun A a := a

Church-encoded boolean type
  $ cat >bools <<EOF
  > let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false := true;
  > let false : Bool := fun Out true false := false;
  > 
  > let not (b : Bool) : Bool := 
  >   fun (Out : Type) (true : Out) (false : Out) := b Out false true;
  > 
  > true Bool false
  > EOF
  $ cat bools | elab-dependent elab
  <input> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  := let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
    let true : Bool := fun Out true false := true;
    let false : Bool := fun Out true false := false;
    let not : fun (b : Bool) -> Bool :=
      fun b Out true false := b Out false true;
    true Bool false
  $ cat bools | elab-dependent norm
  <input> :
    fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
        (Out : Type) (true : Out) (false : Out) -> Out
  := fun false Out true false := false

Church-encoded option type
  $ cat >options <<EOF
  > let Option (A : Type) : Type :=
  >   fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
  > let none (A : Type) : Option A :=
  >   fun Out some none := none;
  > let some (A : Type) (a : A) : Option A :=
  >   fun Out some none := some a;
  > 
  > some (Option Type) (some Type (Type -> Type))
  > EOF
  $ cat options | elab-dependent elab
  <input> :
    fun (Out : Type)
        (some : fun (Out : Type) (some : Type -> Out) (none : Out) -> Out ->
          Out)
        (none : Out) -> Out
  :=
    let Option : fun (A : Type) -> Type :=
      fun A := fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
    let none : fun (A : Type) -> Option A := fun A Out some none := none;
    let some : fun (A : Type) (a : A) -> Option A :=
      fun A a Out some none := some a;
    some (Option Type) (some Type (Type -> Type))
  $ cat options | elab-dependent norm
  <input> :
    fun (Out : Type)
        (some : fun (Out : Type) (some : Type -> Out) (none : Out) -> Out ->
          Out)
        (none : Out) -> Out
  := fun Out some none := some (fun Out some none := some (Type -> Type))

Name not bound
  $ elab-dependent elab <<< "fun (A : Type) (a : A) := foo"
  error: `foo` is not bound in the current scope
  [1]

An example of a type error
  $ elab-dependent elab <<EOF
  > let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false := true;
  > let false : Bool := fun Out true false := false;
  > 
  > true Type : Bool
  > EOF
  error: type mismatch
    expected: fun (Out : Type) (true : Out) (false : Out) -> Out
    found:    fun (true : Type) (false : Type) -> Type
  [1]
