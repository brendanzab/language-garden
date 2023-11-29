Test files
----------

Elaborate test term
  $ cat term.txt | stlc-algebraic
  fun (f : A -> B) => fun (a : A) => f a : (A -> B) -> A -> B

Elaborate identity function test
  $ cat id.txt | stlc-algebraic
  let id : A -> A := fun (x : A) => x; fun (x : A) => id x : A -> A


Elaboration Errors
------------------

Unexpected function literal
  $ stlc-algebraic <<< "(fun a => a) : B"
  <input>:1:0: error: found function, expected `B`
  [1]

Unexpected function literal
  $ stlc-algebraic <<< "(fun (a : A) => a) : B"
  <input>:1:0: error: found function, expected: `B`
  [1]

Unexpected parameter type
  $ stlc-algebraic <<< "(fun (a : A) => a) : B -> B"
  <input>:1:6: error: unexpected parameter type, found `A`, expected: `B`
  [1]

Unbound variable
  $ stlc-algebraic <<< "fun (a : A) => b"
  <input>:1:15: error: unbound variable `b`
  [1]

Type mismatch (FIXME: found/expected order?)
  $ stlc-algebraic <<< "fun (a : A) => a : B"
  <input>:1:15: error: type mismatch, found `B` expected `A`
  [1]

Missing parameter annotation
  $ stlc-algebraic <<< "fun (f : A -> B) => fun a => f a"
  <input>:1:24: error: annotation required
  [1]

Mismatched argument
  $ stlc-algebraic <<< "fun (f : A -> B) => fun (b : B) => f b"
  <input>:1:37: error: mismatched argument type, found `A` expected `B`
  [1]
