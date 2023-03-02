# Simply typed lambda calculus with mutable metavariables

This an elaborator for the simply typed lambda calculus that uses
_metavariables_ to allow programmers to omit type annotations. This approach is
a stepping-stone to more powerful type checking algorithms, such as those for
Hindley-Milner type systems.

This implementation is based on [Arad Arbel’s gist](https://gist.github.com/aradarbel10/837aa65d2f06ac6710c6fbe479909b4c).
It’s not a highly optimised implementation – the goal here is clarity.

## Examples

```sh
$ stlc-unification <<< "1 + 2"
1 + 2 : Int
```

```sh
$ stlc-unification <<< "fun x => x + 2"
fun (x : Int) => x + 2 : Int -> Int
```

```sh
$ stlc-unification <<< "let f := fun x => x; f 3"
let f : Int -> Int := fun (x : Int) => x; f 3 : Int
```
