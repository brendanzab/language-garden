# A small dependently typed language

- Extends [**elab-dependent**](../elab-dependent) (+ syntax sugar)

---

This is a variant of the [elab-dependent](../elab-dependent/) project with
fancier syntactic sugar for functions and let bindings. See that project’s
README for more details and resources.

## Example

<!-- $MDX file=test/readme/bools.txt -->
```
let Bool := fun (Out : Type) (true : Out) (false : Out) -> Out;
let true : Bool := fun Out true false => true;
let false : Bool := fun Out true false => false;

let not (b : Bool) : Bool :=
  fun (Out : Type) (true : Out) (false : Out) => b Out false true;

true Bool false
```

```sh
$ cat ./test/readme/bools.txt | dependent-sugar norm
<stdin> :
  fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
      (Out : Type) (true : Out) (false : Out) -> Out
:=
  fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
  (Out : Type) (true : Out) (false : Out) =>
    false
```
