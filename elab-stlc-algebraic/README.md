# Algebraic elaboration for STLC

When implementing programming languages it’s a good idea to have a small, typed
_core language_ that can be checked trivially.
We then define an _elaborator_ that lowers a higher-level _surface language_ to
the core language.
Unfortunately elaboration can get rather complicated, having to manage both
type checking, type-directed desugaring, error reporting, and lowering to the
core language all at once.

This project explores alleviating some of this burden by taking an LCF-inspired
approach to elaboration, which moves the construction of well-typed terms behind
a trusted interface that exposes a set of typing rules that can be invoked by
the elaborator. Elaboration can then focus on desugaring and error reporting.

## Resources

Presentations:

- Robert Atkey, “An Algebraic Approach to Typechecking and Elaboration”
  ([URL](https://bentnib.org/posts/2015-04-19-algebraic-approach-typechecking-and-elaboration.html))
- Andrej Bauer, “Derivations as computations”, ICFP’19
  ([Video](https://www.youtube.com/watch?v=YZqOVsuyQyQ))
  ([URL](https://math.andrej.com/2019/08/21/derivations-as-computations/))
  ([Slides](https://math.andrej.com/wp-content/uploads/2019/08/derivations-as-computations-icfp-2019.pdf))

Related projects:

- [jonsterling/dreamtt](https://github.com/jonsterling/dreamtt/)
- [RedPRL/cooltt](https://github.com/RedPRL/cooltt/)
- [RedPRL/algaett](https://github.com/RedPRL/algaett/)
- [TOTBWF/MicroTT.ml](https://gist.github.com/TOTBWF/9b2c071d2edb1c6596b785656c866fd6)
- [TOTBWF/teenytt](https://github.com/TOTBWF/teenytt)
