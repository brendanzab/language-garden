# A simple Datalog interpreter

This is an interpreter for Datalog, originally based on the implementation
described in [The Essence of Datalog][essence-of-datalog] by Mistral Contrastin.

The goal was to understand how Datalog accumulates facts in a knowledge base,
and to get a sense of where opportunities for parallelism and other performance
improvements might exist.
The interpreter is extremely naive, using inappropriate data structures and
having none of the standard optimizations implemented, so it’s not at all ready
for production.

[essence-of-datalog]: https://dodisturb.me/posts/2018-12-25-The-Essence-of-Datalog.html
