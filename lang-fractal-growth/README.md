# Fractal Growth

Experiments with using production systems to describe fractal growth.

```command
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

$ fractal-growth generations --system=filament | head --lines=6
(-->)
(<--)(->)
(<-)(-->)(-->)
(<--)(<--)(->)(<--)(->)
(<-)(-->)(<-)(-->)(-->)(<-)(-->)(-->)
(<--)(<--)(->)(<--)(<--)(->)(<--)(->)(<--)(<--)(->)(<--)(->)
```

## Resources

- [Algorithmic Botany](http://algorithmicbotany.org/): Homepage of the Biological
  Modeling and Visualization research group at the University of Calgary.
- “The algorithmic beauty of plants” by Prusinkiewicz and Lindenmayer 1990
  [[URL](http://algorithmicbotany.org/papers/#abop)]
- “Theoretical Pearl: L-systems as Final Coalgebras” by Rein Henrichs 2015
  [[URL](http://reinh.com/notes/posts/2015-06-27-theoretical-pearl-l-systems-as-final-coalgebras.html)]
