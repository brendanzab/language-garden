# Abstract elaboration for STLC

- Extends [**elab-stlc-bidirectional**](../elab-stlc-bidirectional) (+ LCF-style kernel)

---

The translation of a high level, user friendly _surface language_ into a small,
typed _core language_ is a useful pattern for implementing typed programming
languages.
We call this _elaboration_ because it fills in details that were otherwise
implicit, and it is used in the implementations of languages such as GHC Haskell,
Idris and Coq.

Unfortunately elaborators can get rather complicated because type checking,
desugaring and error reporting are all fused together.
This project explores a way to alleviate this burden on the elaborator by
taking an LCF-inspired approach for the core language.
Instead of type checking and constructing core terms within the elaborator, we
instead move this behind trusted inference rules defined in the core
(see [`core.mli`]).
Elaboration no longer needs to interact with core terms directly, and can
instead focus on desugaring and error reporting (see [`surface.ml`]).

[`Core.mli`]: ./core.mli
[`Surface.ml`]: ./surface.ml

## Todo

- [x] Elaboration of lambda terms
- [x] Elaboration tests
- [ ] Unification and metavariables
- [ ] Collect multiple errors during elaboration

## Resources

- [LCF architecture](https://www.pls-lab.org/en/LCF_architecture) on PLS Lab

Presentations:

- Robert Atkey, “An Algebraic Approach to Typechecking and Elaboration”
  ([URL](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html))
- Andrej Bauer, “Derivations as computations”, ICFP’19
  ([Video](https://www.youtube.com/watch?v=YZqOVsuyQyQ))
  ([URL](https://math.andrej.com/2019/08/21/derivations-as-computations/))
  ([Slides](https://math.andrej.com/wp-content/uploads/2019/08/derivations-as-computations-icfp-2019.pdf))
- John Harrison, “The LCF Approach to Theorem Proving”
  ([Slides](https://www.cl.cam.ac.uk/~jrh13/slides/manchester-12sep01/slides.pdf))

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
