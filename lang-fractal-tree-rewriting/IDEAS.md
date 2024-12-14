# Plant growth DSL

A sketch of a DSL that can be used to model plant growth.

- Unlike L-systems which operate on strings, we operate directly on trees,
  which helps to better capture the branching anatomy of plants.
  This avoids the workarounds needed to get context-sensitive L-systems to work with strings.
- I was probably prompted to start re-examining the string rewriting approach that L-systems use
  as a result of reading tweets from the developers of [Idu](https://idu.cyberplant.ee/) expressing skepticism of L-systems.
  I believe they use a markov-chain based approach on graphs.
- It would be interesting to see if people have done something similar,
  but searching for work on parallel rewriting on trees is hard,
  because so many of the results are drowned drowned out by L-systems!
- The anatomy of the plant and how it is rendered are more carefully separated than in L-systems,
  where the symbols sometimes stand-in for turtle drawing commands.
  - This is a cleaner separation of concerns,
    but has the downside of making plants more tedious to define.
  - It would be fun if you could have a library of common plant anatomical elements (along with a “stock’ graphical interpretation) that could be stitched together into a plant.
- I was partly inspired by the so-called “catamorphism” notation in the [Nanopass Framework](https://nanopass.org) to elide boilerplate rules,
  and to automate threading through transformations.
- I’m still not entirely happy with the notation... L-systems are still a lot cleaner.
- I still don’t know how the rules would be wired together at the top level
- Feeding information back into the model, e.g. gravity, light, world geometry
- Would like to make an editor with an actual DSL implementation

Previous experiments:

- [fractal_growth.ml](https://gist.github.com/brendanzab/d614ca12319a57bd5a77e08094cd20e4)
- [lindenmayer_systems.ml](https://gist.github.com/brendanzab/bb5ea233c010f0bff2461c35fa89367c)
- [lang-fractal-growth](https://github.com/brendanzab/language-garden/tree/main/lang-fractal-growth) in my [language-garden](https://github.com/brendanzab/language-garden)
- a larger prototype in OCaml, implemented with a custom recursion scheme library and diagrams library (still need to publish this!)

## Resources

- [Algorithmic Botany](https://web.archive.org/web/20240924020925/http://algorithmicbotany.org/)
- [L-system](https://en.wikipedia.org/wiki/L-system) on Wikipedia
- [Lindenmayer Systems, Coalgebraically](https://coalg.org/cmcs12/slides/baltasar_winter.pdf)
- [Theoretical Pearl: L-systems as Final Coalgebras](https://web.archive.org/web/20221205050615/http://reinh.com/notes/posts/2015-06-27-theoretical-pearl-l-systems-as-final-coalgebras.html)
- [cyberplant.xyz: June 3, 2020](https://cyberplant.xyz/posts/may/)

## Examples

The following examples are derived from L-systems.

### Algae

Model of a single filament of algae[^algae]

```text
data cell :=
  | .large
  | .small

data filament :=
  | .cons(cell, filament)
  | .nil

def axiom :=
  .cons(.small, .nil)

def rules :=
  | .cons(.large, ...) -> .cons(.large, .cons(.small, ...))
  | .cons(.small, ...) -> .cons(.large, ...)
```

[^algae]: Figure 1.3 in “The Algorithmic Beauty of Plants”

### Anabaena Cetenula

A model of a filament of Anabaena Cetenula[^anabaena1][^anabaena2][^anabaena3].

```text
data polarity :=
  | .left
  | .right

def length :=
  | .short
  | .long

data cell :=
  | .cell(.length, .polarity)

data filament :=
  | .cons(cell, filament)
  | .nil

def axiom :=
  .cons(.cell(.long, .left), .nil)

def rules :=
  | .cons(.cell(.short, pol), ...) -> .cons(.cell(.long, pol), ...)
  | .cons(.cell(.long, .left), ...) -> .cons(.cell(.short, .left), .cons(.cell(.long, .right), ...))
  | .cons(.cell(.long, .right), ...) -> .cons(.cell(.long, .left), .cons(.cell(.short, .right), ...))
```

[^anabaena1]: Section 1.2 of “The Algorithmic Beauty of Plants”
[^anabaena2]: Section 3 of “Introduction to Modeling with L-systems” <http://algorithmicbotany.org/papers/sigcourse.2003/1-9-L-fundamentals.pdf>
[^anabaena3]: Mitchison and Wilcox, “Rule governing Cell Division in Anabaena”, Nature, 239:110-111, 1972

### Anabaena Cetenula (detailed)

A more detailed model of a filement of Anabaena Cetenula[^anabaena4].

```text
data polarity :=
  | .left
  | .right

data cell :=
  | .cell(int, polarity)

data filament :=
  | .cons(cell, filament)
  | .nil

def axiom :=
  .cons(.cell(age_div, .left), .nil)

def rules :=
  | .cons(.cell(age, pol), ...) if age < 5 ->
      .cons(.cell(age + 1, pol), ...)
  | .cons(.cell(age, .left), ...) if age >= 5 ->
      .cons(.cell(1, .left), .cons(.cell(2, .right), ...))
  | .cons(.cell(age, .right), ...) if age >= 5 ->
      .cons(.cell(2, .left), .cons(.cell(1, .right), ...))
```

[^anabaena4]: Section 4 of “Introduction to Modeling with L-systems” <http://algorithmicbotany.org/papers/sigcourse.2003/1-9-L-fundamentals.pdf>

### Binary tree

A fractal binary tree[^binary-tree].

```text
data tree :=
  | .apex
  | .fork(tree, tree)
  | .branch(tree)

def axiom :=
  .apex

def rules :=
  | .apex -> .branch(.fork(.apex, .apex))
  | .branch(...) -> .branch(.branch(...))
```

[^binary-tree]: [L-system: Example 2: Fractal (binary) tree](https://en.wikipedia.org/wiki/L-system#Example_2:_fractal_(binary)_tree) on Wikipedia.

### Binary tree parameterised

Fractal binary tree with a branch length parameter.

```tree
data tree :=
  | .apex
  | .fork(tree, tree)
  | .branch(float, tree)

def axiom :=
  .apex

def rules :=
  | .apex -> .branch(.fork(.apex, .apex))
  | .branch(len, ...) -> .branch(len * 1.5., ...)
```

### Monopoidal inflorescence

TODO

Show the composition of multiple growth systems.

### Non-deterministic branching

An example of allowing a non-deterministic selection of rules to be selected[^nondet].

```text
data tree :=
  | .apex
  | .fork(tree, tree)
  | .left(tree)
  | .right(tree)
  | .branch(tree)

def axiom :=
  .branch(.apex)

def rules : _ -> nondet _ :=
  | .branch(...) ->
      one-of [
        -- branch left and right:
        --
        -- o :
        --  \| o
        --   |/
        --   |
        .branch(.fork(.right(.branch(.apex)),
          .branch(.fork(.left(.branch(.apex)),
            .branch(...))))),

        -- branch right:
        --
        --   : o
        --   |/
        --   |
        .branch(.fork(.right(.branch(.apex)),
          .branch(...))),

        -- branch left:
        --
        -- o :
        --  \|
        --   |
        .branch(.fork(.left (.branch(.apex)),
          .branch(...)));
      ]
```

[^nondet]: Section 1.7 of “The Algorithmic Beauty of Plants”

### Stochastic branching

A variant of non-determinism where frequencies can be selected.

TODO

- Section 1.7 of “The Algorithmic Beauty of Plants”

### Branch shedding

Example of branch shedding[^branch-shedding].

```text
module branch := {

  type age := int

  data t :=
    | .apex           -- where new growth is introduced
    | .scar           -- added when a branch is shed
    | .internode(t)
    | .base(age, t)

  def axiom :=
    .base(0, .apex)

  def rules :=
    | .apex -> .internode(.apex)
    | .base(age, ...) if age > 3 -> .base(age + 1, .internode(.scar))
    | .base(age, ...) if age <= 3 -> .base(age + 1, ...)

}

module tree := {

  data t :=
    | .apex
    | .trunk(t)
    | .fork(branch.t, t, branch.t)

  def axiom := .apex

  def rules :=
    | .apex -> .trunk(.fork(branch.axiom, .apex, branch.axiom))
    -- TODO: apply `branch.rules`?

}
```

[^branch-shedding]: Section 6.5 of “L-systems: from the Theory to Visual Models of Plants” by Prusinkiewicz et. al.

### Signal propagation over binary trees

This demonstrated context sensitive signal propagation over binary trees[^context-sensitive].
This can be used to model chemical signalling over plants.
Here, the `<` and `>` symbols are used to match on information being propagated up and down the tree.

Acropetal (root-to-apex) signalling:

```text
data signal :=
  | .on
  | .off

data tree :=
  | .apex
  | .fork(tree, tree)
  | .left(tree)
  | .right(tree)
  | .branch(signal, tree)

def axiom :=
  .branch(.on, .fork(
    .left(.branch(.off, .apex)),
    .branch(.off, .fork(
      .right(.branch(.off, .apex)),
      .branch(.off, .fork(
        .left(.branch(.off, .apex)),
        .branch(.off, .apex),
      )),
    )),
  ))

def < :=
  | .branch(s, _) -> s

def rules : signal < _ -> _ :=
  | .on < .branch(.off, ...) -> .branch(.on, ...)
```

Basipetal (apex-to-root) signalling:

```text
data signal :=
  | .on
  | .off

def axiom :=
  .branch(.off, .fork(
    .left(.branch(.off, .apex)),
    .branch(.off, .fork(
      .right(.branch(.off, .apex)),
      .branch(.off, .fork(
        .left(.branch(.off, .apex)),
        .branch(.on, .apex),
      )),
    )),
  ))

data tree :=
  | .apex
  | .fork(tree, tree)
  | .left(tree)
  | .right(tree)
  | .branch(signal, tree)

def > :=
  | .apex -> .off
  | .right(s) | .left(s) -> s
  | .fork(.on, _) -> .on
  | .fork(_, .on) -> .on
  | .fork(_, _) -> .off
  | .branch(s, _) -> s

def rules : _ > signal -> _ :=
  | .branch(.off, ... > .on) -> .branch(.on, ...)
```

[^context-sensitive]: Section 1.8 of “The Algorithmic Beauty of Plants”.
