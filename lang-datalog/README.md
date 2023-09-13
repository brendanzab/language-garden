# A simple Datalog interpreter

This is an interpreter for [Datalog][datalog], originally based on the
implementation described in [The Essence of Datalog][essence-of-datalog] by
Mistral Contrastin.

The goal was to understand how Datalog accumulates facts in a knowledge base,
and to get a sense of where opportunities for parallelism and other performance
improvements might exist.
The interpreter is extremely naive, using inappropriate data structures and
having none of the standard optimizations implemented, so it’s not at all ready
for production.

[datalog]: https://en.wikipedia.org/wiki/Datalog
[essence-of-datalog]: https://dodisturb.me/posts/2018-12-25-The-Essence-of-Datalog.html

## Example

A simple example of defining and querying a graph using Datalog:

<!-- $MDX file=examples/graph.datalog -->
```datalog
path(X, Y) <- edge(X, Y).
path(X, Y) <- edge(X, Z), edge(Z, Y).

edge(1, 2).
edge(2, 3).

? path(X, 3).
? path(X, Y).
```

```sh
$ cat examples/graph.datalog | datalog
────────────────────────────────────────────────────────────────────────────────
Knowledge Base
────────────────────────────────────────────────────────────────────────────────
edge(1, 2).
edge(2, 3).
path(1, 2).
path(2, 3).
path(1, 3).

────────────────────────────────────────────────────────────────────────────────
Query Results
────────────────────────────────────────────────────────────────────────────────
? path(X, 3).
  > X := 2.
  > X := 1.
  yes

? path(X, Y).
  > X := 1.
    Y := 2.
  > X := 2.
    Y := 3.
  > X := 1.
    Y := 3.
  yes

```

## Additional Resources

Here’s some additional resources on Datalog that looked interesting, but I
haven’t had time to go through yet:

- [“Foundations of databases”](http://webdam.inria.fr/Alice/)
  by Abiteboul, Hull, and Vianu
- [“Datalog - a precursor to Prolog”](https://nickelsworth.github.io/sympas/16-datalog.html)
  by Manfred von Thun from “Symbolic Processing In Pascal”
- [“A relatively simple Datalog engine in Rust”](https://github.com/frankmcsherry/blog/blob/master/posts/2018-05-19.md)
  by Frank McSherry
- [“Codebase as Database: Turning the IDE Inside Out with Datalog”](https://petevilter.me/post/datalog-typechecking/)
  by Pete Vilter
