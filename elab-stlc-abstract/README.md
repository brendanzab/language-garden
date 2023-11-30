# Abstract elaboration for STLC

A useful pattern for implementing typed programming languages is to translate a
high level, user friendly _surface language_ into a small, typed _core language_
that is close to well-understood type theories.
Because it fills in details that were otherwise implicit, we call this process
_elaboration_, and you can find it used in the implementations of languages such
as GHC Haskell, Idris and Coq.
Unfortunately elaboration can get rather complicated, because type-directed
desugaring, type checking and error reporting are all fused together.

This project explores a way to alleviate this burden on the elaborator by
taking an LCF-inspired approach.
Instead of type checking and constructing core terms within the elaborator, we
instead move the construction of well-typed terms behind trusted inference rules
defined in the core.
Elaboration no longer needs to interact with core terms directly, and can
instead focus on desugaring and error reporting.

## Todo

- [x] Elaboration of lambda terms
- [x] Elaboration tests
- [ ] Unification and metavariables
- [ ] Collect multiple errors during elaboration

## Resources

Presentations:

- Robert Atkey, “An Algebraic Approach to Typechecking and Elaboration”
  ([URL](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html))
- Andrej Bauer, “Derivations as computations”, ICFP’19
  ([Video](https://www.youtube.com/watch?v=YZqOVsuyQyQ))
  ([URL](https://math.andrej.com/2019/08/21/derivations-as-computations/))
  ([Slides](https://math.andrej.com/wp-content/uploads/2019/08/derivations-as-computations-icfp-2019.pdf))

Related projects:

- [jonsterling/dreamtt](https://github.com/jonsterling/dreamtt/):
  Pedagogic abstract elaboration for dependent type theory.
- [RedPRL/cooltt](https://github.com/RedPRL/cooltt/):
  Elaboration for Cartesian cubical type theory.
- [RedPRL/algaett](https://github.com/RedPRL/algaett/):
  Elaborator for dependent type theory using effects and handlers.
- [TOTBWF/MicroTT.ml](https://gist.github.com/TOTBWF/9b2c071d2edb1c6596b785656c866fd6):
  A simple single-file elaborator for MLTT.
- [TOTBWF/teenytt](https://github.com/TOTBWF/teenytt)
  A very small, didactic proof assistant for dependent type theory.
