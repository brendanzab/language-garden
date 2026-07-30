Record literal
  $ executable elab <<< "{ x := 1; y := 2; foo := { bar := true } }"
  { foo := { bar := true }; x := 1; y := 2 } :
    { foo : { bar : Bool }; x : Int; y : Int }

Record projection
  $ executable elab <<EOF 
  >   fun (r : { foo : { bar : Bool }; x : Int; y : Int }) => 
  >     { fst := r.foo.bar; snd := r.x }
  > EOF
  fun (r : { foo : { bar : Bool }; x : Int; y : Int }) =>
    { fst := r.foo.bar; snd := r.x }
  : { foo : { bar : Bool }; x : Int; y : Int } -> { fst : Bool; snd : Int }

--------------------------------------------------------------------------------

Unknown field
  $ executable elab <<< "{ x := 42 }.y"
  error: unknown field `y`
    ┌─ <stdin>:1:0
    │
  1 │ { x := 42 }.y
    │ ^^^^^^^^^^^
    = expected: { .. y : _ }
         found: { x : Int }
  
  [1]

Unexpected projection
  $ executable elab <<< "true.y"
  error: unknown field `y`
    ┌─ <stdin>:1:0
    │
  1 │ true.y
    │ ^^^^
    = expected: { .. y : _ }
         found: Bool
  
  [1]

Duplicate label
  $ executable elab <<< "{ x := 42; x := 2 }"
  error: duplicate label `x`
    ┌─ <stdin>:1:11
    │
  1 │ { x := 42; x := 2 }
    │            ^
  
  [1]

Merging two open rows
  $ executable elab <<< "fun x y => let _ := x.a + 1; let _ := y.b + 1; if true then x else y"
  fun (x : { a : Int; b : Int }) => fun (y : { a : Int; b : Int }) =>
    let _ : Int := #int-add x.a 1;
    let _ : Int := #int-add y.b 1;
    if true then x else y
  : { a : Int; b : Int } -> { a : Int; b : Int } -> { a : Int; b : Int }

Infinite type via a row constraint (regression test for an occurs check
that previously skipped the row constraints of unsolved row metavariables,
constructing a cyclic type that diverged during printing)
  $ executable elab <<< "fun x => if true then x.self else x"
  error: infinite type
    ┌─ <stdin>:1:34
    │
  1 │ fun x => if true then x.self else x
    │                                   ^
  
  [1]

Infinite row type when merging two open rows (regression test for the
reverse-direction occurs check when unifying two unsolved row metavariables)
  $ executable elab <<< "fun x y => let _ := if true then x.a else y; let _ := y.b + 1; if true then x else y"
  error: infinite row type
    ┌─ <stdin>:1:83
    │
  1 │ fun x y => let _ := if true then x.a else y; let _ := y.b + 1; if true then x else y
    │                                                                                    ^
  
  [1]
