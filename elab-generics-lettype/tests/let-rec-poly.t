Initial setup
  $ alias executable=generics-lettype

--------------------------------------------------------------------------------

Factorial in terms of the fixed-point combinator
  $ cat >fix.txt <<EOF
  > let rec fix [A, B] (f : (A -> B) -> A -> B) (x : A) : B :=
  >   f (fix f) x;
  > 
  > let fact n :=
  >   fix (fun fact n =>
  >     if n = 0 then 1 else n * fact (n - 1)) n;
  > 
  > fact 5
  > EOF

  $ cat fix.txt | executable elab
  let rec {
    fix [A, B] : ((A -> B) -> A -> B) -> A -> B :=
      fun (f : (A -> B) -> A -> B) => fun (x : A) => f (fix [A, B] f) x;
  };
  let fact : Int -> Int :=
    fun (n : Int) =>
      fix
        [Int, Int]
        (fun (fact : Int -> Int) => fun (n : Int) =>
           if #int-eq n 0 then 1 else #int-mul n (fact (#int-sub n 1)))
        n;
  fact 5 : Int
  $ cat fix.txt | executable eval
  120 : Int
