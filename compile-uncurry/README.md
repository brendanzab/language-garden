# Uncurrying lambda expressions

> NOTE: A work in progress!

This is an attempt to compile a language where all functions take a single
argument to a language where functions take multiple arguments. The intention is
to explore ways of supporting uncurrying for DSLs that compile to high-level
languages with multiparameter functions.

I didn’t really follow any technical resources when implementing this, just
attempted to implement things myself beginning with what I saw in [Christoph
Breitkopf’s implementation][christoph-breitkopf-icfp2014], extending it to
handle applications on variables.

## Resources

As far as I can tell, uncurrying is often done as a part of other
transformations, like closure conversion, and so it can be hard to find specific
resources and examples that explain this specific transformation in isolation,
or in much depth.

ReScript apparently has [very good support for high level uncurrying][rescript-uncurrying],
targeting high-level Javascript, (seen formerly in Bucklescript). Unfortunately
I’ve not been able to figure out where this is actually implemented and figure
out how it works.

As mentioned previously, Christoph Breitkopf implemented a [simple form of
uncurrying][christoph-breitkopf-icfp2014] that does not handle partial
applications as part of a contest entry for ICFP 2014.

Lower level approaches to uncurrying can be found in Johan Nordlander’s slides
for their 2011 course, [Compiling Functional Languages][compfun-2011].
[Lecture 2][compfun-2011-lecture2] (from page 17) is relevant, although I hear
that it might be preferrable to avoid the lambda lifting that it employs.
[Lecture 6][compfun-2011-lecture6] shows an interesting approach that uses
types to keep track of function arities.

[Making a fast curry: push/enter vs. eval/apply for higher-order languages][fast-curry]
is a famous paper that explores how to efficiently handle the troublesome cases
of partial application where the arity of arguments are unknown, but doesn’t
go through how to implement the easy cases, as far as I can tell! See also
Xavier Leroy’s commentary on this work in [these slides][zam-kazam05].

[christoph-breitkopf-icfp2014]: https://github.com/bokesan/icfpc2014/blob/942a208d071b9d18511b99423aae789e725c1483/compiler/gcc/Compile.hs
[rescript-uncurrying]: https://rescript-lang.org/docs/manual/v8.0.0/bind-to-js-function
[compfun-2011]: https://www.cse.chalmers.se/edu/year/2011/course/CompFun
[compfun-2011-lecture2]: https://www.cse.chalmers.se/edu/year/2011/course/CompFun/lecture2.pdf
[compfun-2011-lecture6]: https://www.cse.chalmers.se/edu/year/2011/course/CompFun/lecture6.pdf
[fast-curry]: https://doi.org/10.1017/S0956796806005995
[zam-kazam05]: https://xavierleroy.org/talks/zam-kazam05.pdf
