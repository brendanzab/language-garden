# A small dependently typed language

- Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional) (+ dependent types)

---

This is an implementation of a small dependently typed language where types are
first-class and where the return type of a function may depend on the arguments
it is applied to.

Type checking is is implemented in terms of an _elaborator_, which checks and
tanslates a user-friendly _surface language_ into a simpler and more explicit
_core language_ that is more closely connected to type theory. Because we need
to simplify types during elaboration we also implement an interpreter for the
core language.

*This was originally posted at
[tt-hoas-nameless.ml](https://gist.github.com/brendanzab/a8443e9d267ed142e4603fc3cb5fa9c8).*

## Example

<!-- $MDX file=test/readme/bools.txt -->
```
let Bool : Type := fun (Out : Type) (true : Out) (false : Out) -> Out;
let true : Bool := fun Out true false => true;
let false : Bool := fun Out true false => false;

let not : Bool -> Bool := fun b =>
  fun Out true false => b Out false true;

true Bool false
```

```sh
$ cat ./test/readme/bools.txt | dependent norm
<stdin> :
  fun (false : fun (Out : Type) (true : Out) (false : Out) -> Out)
      (Out : Type) (true : Out) (false : Out) -> Out
:= fun false Out true false := false
```

## Some useful resources

- [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/):
  Some excellent, high quality examples of implementing elaborators for
  dependently typed programming languages, demonstrating bidirectional type
  checking and normalisation-by-evaluation, and extensions to this.
- mb64’s excellent gists showing tiny type system implementations:
  - [mlt.ml](https://gist.github.com/mb64/4a49d710dcdd1875bebdbc59081acb85)
  - [tychk_nbe.ml](https://gist.github.com/mb64/814836449f60b05113885fe93068bf1d)
- [A Tutorial Implementation of a Dependently Typed Lambda Calculus](https://www.andres-loeh.de/LambdaPi/)
  by Andres Löh, Conor McBride and Wouter Swierstra:
  A little outdated compared to the above implementations, but the paper is
  still worth a look, showing how to incrementally add dependent types to a
  simply typed lambda calculus.
- [nbe-for-mltt](https://github.com/jozefg/nbe-for-mltt): An implementation of
  bidirectional typechecking and normalisation-by-evaluation for dependent
  types. Some more information on the implementation can be found in the paper,
  [Implementing a Modal Dependent Type Theory](https://dl.acm.org/doi/10.1145/3341711)
  by Daniel Gratzer, Jonathan Sterling and Lars Berkedal.
- [Checking Dependent Types with Normalization by Evaluation: A Tutorial](https://davidchristiansen.dk/tutorials/nbe/)
  by David Thrane Christiansen: Implements a dependent typechecker using
  bidirectional type checking and normalisation-by-evaluation.
