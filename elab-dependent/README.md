# A small dependently typed language

This is an implementation of a small dependently typed language where types are
first-class and where the return type of a function may depend on the arguments
it is applied to.

*This was originally posted at
[tt-hoas-nameless.ml](https://gist.github.com/brendanzab/a8443e9d267ed142e4603fc3cb5fa9c8).*

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
