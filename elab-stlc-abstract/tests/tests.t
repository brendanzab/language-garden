Test files
----------

Elaborate test term
  $ cat term.txt | stlc-abstract
  fun (f : A -> B) => fun (a : A) => f a : (A -> B) -> A -> B

Elaborate identity function test
  $ cat id.txt | stlc-abstract
  let id : A -> A := fun (x : A) => x;
  fun (x : A) => id x : A -> A


Elaboration Errors
------------------

Unexpected function literal
  $ stlc-abstract <<< "(fun a => a) : B"
  error: found function, expected `B`
    ┌─ <stdin>:1:0
    │
  1 │ (fun a => a) : B
    │ ^^^^^^^^^^^^
  [1]

Unexpected function literal
  $ stlc-abstract <<< "(fun (a : A) => a) : B"
  error: found function, expected `B`
    ┌─ <stdin>:1:0
    │
  1 │ (fun (a : A) => a) : B
    │ ^^^^^^^^^^^^^^^^^^
  [1]

Unexpected parameter type
  $ stlc-abstract <<< "(fun (a : A) => a) : B -> B"
  error: mismatched parameter type, found `A` expected `B`
    ┌─ <stdin>:1:0
    │
  1 │ (fun (a : A) => a) : B -> B
    │ ^^^^^^^^^^^^^^^^^^
  [1]

Unbound variable
  $ stlc-abstract <<< "fun (a : A) => b"
  error: unbound variable `b`
    ┌─ <stdin>:1:15
    │
  1 │ fun (a : A) => b
    │                ^
  [1]

Type mismatch
  $ stlc-abstract <<< "fun (a : A) => a : B"
  error: type mismatch, found `A` expected `B`
    ┌─ <stdin>:1:15
    │
  1 │ fun (a : A) => a : B
    │                ^
  [1]

Missing parameter annotation
  $ stlc-abstract <<< "fun (f : A -> B) => fun a => f a"
  error: annotation required
    ┌─ <stdin>:1:24
    │
  1 │ fun (f : A -> B) => fun a => f a
    │                         ^
  [1]

Mismatched argument
  $ stlc-abstract <<< "fun (f : A -> B) => fun (b : B) => f b"
  error: mismatched argument type, found `B` expected `A`
    ┌─ <stdin>:1:37
    │
  1 │ fun (f : A -> B) => fun (b : B) => f b
    │                                      ^
  [1]
