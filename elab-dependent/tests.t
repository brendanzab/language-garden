The identity function
  $ cat >id <<< "fun (A : Type) (a : A) := a"
  $ cat id | elab-dependent elab
  <input> : fun (A : Type) -> A -> A :=
    (fun A := fun a := a) : fun (A : Type) -> A -> A
  $ cat id | elab-dependent norm
  <input> : fun (A : Type) -> A -> A :=
    fun A := fun a := a

Church-encoded boolean type
  $ cat >bools <<EOF
  > let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false := true;
  > let false : Bool := fun Out true false := false;
  > 
  > true Bool false
  > EOF
  $ cat bools | elab-dependent elab
  <input> : fun (Out : Type) -> Out -> Out -> Out -> fun (Out : Type) -> Out -> Out -> Out :=
    let Bool := fun (Out : Type) -> Out -> Out -> Out; let true : Bool := fun Out := fun true := fun false := true; let false : Bool := fun Out := fun true := fun false := false; true Bool false
  $ cat bools | elab-dependent norm
  <input> : fun (Out : Type) -> Out -> Out -> Out -> fun (Out : Type) -> Out -> Out -> Out :=
    fun false := fun Out := fun true := fun false := false

Church-encoded option type
  $ cat >options <<EOF
  > let Option : Type -> Type :=
  >   fun A := fun (Out : Type) (some : A -> Out) (none : Out) -> Out;
  > let none : fun (A : Type) -> Option A :=
  >   fun A := fun Out some none := none;
  > let some : fun (A : Type) -> A -> Option A :=
  >   fun A a := fun Out some none := some a;
  > 
  > some (Option Type) (some Type (Type -> Type))
  > EOF
  $ cat options | elab-dependent elab
  <input> : fun (Out : Type) -> fun (Out : Type) -> Type -> Out -> Out -> Out -> Out -> Out -> Out :=
    let Option : Type -> Type := fun A := fun (Out : Type) -> A -> Out -> Out -> Out; let none : fun (A : Type) -> Option A := fun A := fun Out := fun some := fun none := none; let some : fun (A : Type) -> A -> Option A := fun A := fun a := fun Out := fun some := fun none := some a; some (Option Type) (some Type (Type -> Type))
  $ cat options | elab-dependent norm
  <input> : fun (Out : Type) -> fun (Out : Type) -> Type -> Out -> Out -> Out -> Out -> Out -> Out :=
    fun Out := fun some := fun none := some (fun Out := fun some := fun none := some (Type -> Type))

An example of a type error
  $ cat >bools <<EOF
  > let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
  > let true : Bool := fun Out true false := true;
  > let false : Bool := fun Out true false := false;
  > 
  > true Type : Bool
  > EOF
  $ cat bools | elab-dependent elab
  error: type mismatch: expected `fun (Out : Type) -> Out -> Out -> Out`, found `Type -> Type -> Type`
  [1]
