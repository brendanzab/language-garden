Elaborate test term
  $ cat term.txt | stlc-algebraic norm
  fun (f : A -> B) => fun (a : A) => f a : (A -> B) -> A -> B

Elaborate identity function test
  $ cat id.txt | stlc-algebraic norm
  let id : A -> A := fun (x : A) => x; fun (x : A) => id x : A -> A
