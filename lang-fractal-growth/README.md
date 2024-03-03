# Fractal Growth

This project aims to experiment with using grammars and rewriting systems (for
example [L-systems]) to describe fractal growth.

I’ve long been curious about grammars and rewriting systems for procedural
generation for a long time, but have found the resources related to L-systems
and production systems a challenge for me to understand. This project is an
attempt to express these ideas in a way that makes more sense to me.

The output of various systems can be found in the [CLI tests](./test/cli.t).
In the future is would be nice to produce graphical output for these systems.

[L-systems]: https://en.wikipedia.org/wiki/L-system

## Examples

[lib/Systems/Algae.ml](./lib/Systems/Algae.ml):

<!-- $MDX file=lib/Systems/Algae.ml,part=grammar -->
```ocaml
(** Cytological state of a cell *)
type symbol =
  | A             (** Long cell, ready to divide *)
  | B             (** Short cell *)

let axiom =
  [B]             (* Seed with a small cell *)

let rules =
  function
  | A -> [A; B]   (* Divide *)
  | B -> [A]      (* Grow *)
```

```sh
$ fractal-growth generations --system=algae | head
b
a
ab
aba
abaab
abaababa
abaababaabaab
abaababaabaababaababa
abaababaabaababaababaabaababaabaab
abaababaabaababaababaabaababaabaababaababaabaababaababa
```

[lib/Systems/Filament.ml](./lib/Systems/Filament.ml):

<!-- $MDX file=lib/Systems/Filament.ml,part=grammar -->
```ocaml
(** Cytological state of a cell *)
type size =
  | A   (** Long cell, ready to divide *)
  | B   (** Short cell *)

(** Where new cells will be produced *)
type polarity =
  | L   (** Divide to the left *)
  | R   (** Divide to the right *)

(** The state of a cell in a filament of Anabaena catenula *)
type symbol = size * polarity

let axiom = [A, R]

let rules =
  function
  | A, R -> [A, L; B, R]    (* Divide right *)
  | A, L -> [B, L; A, R]    (* Divide left *)
  | B, R -> [A, R]          (* Grow right *)
  | B, L -> [A, L]          (* Grow left *)
```

```sh
$ fractal-growth generations --system=filament | head --lines=6
(-->)
(<--)(->)
(<-)(-->)(-->)
(<--)(<--)(->)(<--)(->)
(<-)(-->)(<-)(-->)(-->)(<-)(-->)(-->)
(<--)(<--)(->)(<--)(<--)(->)(<--)(->)(<--)(<--)(->)(<--)(->)
```

## Ideas for future work

L-System variations:

- [ ] Stochastic grammars
- [ ] Context sensitive grammars
- [x] Parametric grammars
- [ ] Shape grammars

For more details see the section on [Wikipedia](https://en.wikipedia.org/wiki/L-system#Variations).

DSL features:

- Tagless final embedded DSL
- External DSL for runtime experimentation
- Browser based playground

CLI features:

- `--axiom=WORD` to set the initial axiom
- `--param=NAME:VALUE` for setting system-specific parameters like `dist` and `angle`
- `render --target=OUTPUT --iter=N` to render to outputs like SVG, Graphvis, etc.
  Could even make use [lang-shader-graphics](../lang-shader-graphics/) for
  rendering to PPM or compiling to GLSL.

## Resources

- [Algorithmic Botany](http://algorithmicbotany.org/): Homepage of the Biological
  Modeling and Visualization research group at the University of Calgary.
- “The algorithmic beauty of plants” by Prusinkiewicz and Lindenmayer 1990
  [[URL](http://algorithmicbotany.org/papers/#abop)]
- “Theoretical Pearl: L-systems as Final Coalgebras” by Rein Henrichs 2015
  [[URL](http://reinh.com/notes/posts/2015-06-27-theoretical-pearl-l-systems-as-final-coalgebras.html)]
