Eta conversion for functions
  $ executable elab <<EOF
  > let Eq : fun (A : Type) -> A -> A -> Type :=
  >   fun A x y => fun (P : A -> Type) -> P x -> P y;
  > 
  > let refl : fun (A : Type) (x : A) -> Eq A x x :=
  >   fun A x P px => px;
  > 
  > let tests : fun (A : Type) -> Type :=
  >   fun A =>
  >     let F1 : Type := A -> A;
  >     let F2 : Type := A -> A -> A;
  > 
  >     -- Eta-conversion for single argument functions
  >     let test : fun (f : F1) -> Eq F1 (fun x => f x) f := fun f => refl F1 f;
  >     let test : fun (f : F1) -> Eq F1 f (fun x => f x) := fun f => refl F1 f;
  > 
  >     -- Eta-conversion for multi-argument functions
  >     let test : fun (f : F2) -> Eq F2 (fun x y => f x y) f := fun f => refl F2 f;
  >     let test : fun (f : F2) -> Eq F2 f (fun x y => f x y) := fun f => refl F2 f;
  > 
  >     Type;
  > 
  > Type
  > EOF
  <stdin> : Type :=
    let Eq : fun (A : Type) A -> A -> Type :=
      fun A x y => fun (P : A -> Type) P x -> P y;
    let refl : fun (A : Type) (x : A) -> Eq A x x := fun A x P px => px;
    let tests : fun (A : Type) -> Type :=
      fun A =>
        let F1 : Type := A -> A;
        let F2 : Type := A -> A -> A;
        let test : fun (f : F1) -> Eq F1 (fun x => f x) f :=
          fun f => refl F1 f;
        let test : fun (f : F1) -> Eq F1 f (fun x => f x) :=
          fun f => refl F1 f;
        let test : fun (f : F2) -> Eq F2 (fun x y => f x y) f :=
          fun f => refl F2 f;
        let test : fun (f : F2) -> Eq F2 f (fun x y => f x y) :=
          fun f => refl F2 f;
        Type;
    Type

Swapping arguments prevents eta conversion
  $ executable elab <<EOF
  > let Eq : fun (A : Type) -> A -> A -> Type :=
  >   fun A x y => fun (P : A -> Type) -> P x -> P y;
  > 
  > let refl : fun (A : Type) (x : A) -> Eq A x x :=
  >   fun A x P px => px;
  > 
  > let tests : fun (A : Type) -> Type :=
  >   fun A =>
  >     let F2 : Type := A -> A -> A;
  > 
  >     let test : fun (f : F2) -> Eq F2 (fun x y => f y x) f := fun f => refl F2 f;
  >     --                                           ^^^^^ NOTE: swapped arguments
  > 
  >     Type;
  > 
  > Type
  > EOF
  error: mismatched types
     ┌─ <stdin>:11:70
     │
  11 │     let test : fun (f : F2) -> Eq F2 (fun x y => f y x) f := fun f => refl F2 f;
     │                                                                       ^^^^^^^^^
     = expected: fun (P : (A -> A -> A) -> Type) P (fun x y => f y x) -> P f
          found: fun (P : (A -> A -> A) -> Type) P f -> P f
  
  [1]
