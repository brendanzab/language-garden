Usage
  $ fractal-growth
  fractal-growth: required COMMAND name is missing, must be either 'generations' or 'list'.
  Usage: fractal-growth COMMAND …
  Try 'fractal-growth --help' for more information.
  [124]

List of systems
  $ fractal-growth list
  algae
  filament

Algae generations
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

Filament generations
  $ fractal-growth generations --system=filament | head --lines=6
    (-->)
    (<--)(->)
    (<-)(-->)(-->)
    (<--)(<--)(->)(<--)(->)
    (<-)(-->)(<-)(-->)(-->)(<-)(-->)(-->)
    (<--)(<--)(->)(<--)(<--)(->)(<--)(->)(<--)(<--)(->)(<--)(->)
