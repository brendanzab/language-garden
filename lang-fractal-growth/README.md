# Fractal Growth

Experiments with using production systems to describe fractal growth.

```text
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

```text
$ fractal-growth generations --system=filament | head --lines=6
(-->)
(<--)(->)
(<-)(-->)(-->)
(<--)(<--)(->)(<--)(->)
(<-)(-->)(<-)(-->)(-->)(<-)(-->)(-->)
(<--)(<--)(->)(<--)(<--)(->)(<--)(->)(<--)(<--)(->)(<--)(->)
```

The output of various systems can be found in the [CLI tests](./test/cli.t).
In the future is would be nice to produce graphical output for these systems.

## Ideas for future work

L-System variations:

- Stochastic grammars
- Context sensitive grammars
- Parametric grammars
- Shape grammars

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
