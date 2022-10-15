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
  koch-island
  binary-tree
  cantor-set

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

Koch Island
  $ fractal-growth generations --system=koch-island | head --lines=2
    F-F-F-F
    F-F+F+FF-F-F+F-F-F+F+FF-F-F+F-F-F+F+FF-F-F+F-F-F+F+FF-F-F+F

Binary tree
  $ fractal-growth generations --system=binary-tree | head --lines=5
    0
    1[0]0
    11[1[0]0]1[0]0
    1111[11[1[0]0]1[0]0]11[1[0]0]1[0]0
    11111111[1111[11[1[0]0]1[0]0]11[1[0]0]1[0]0]1111[11[1[0]0]1[0]0]11[1[0]0]1[0]0

Cantor set
  $ fractal-growth generations --system=cantor-set | head --lines=5
    A
    ABA
    ABABBBABA
    ABABBBABABBBBBBBBBABABBBABA
    ABABBBABABBBBBBBBBABABBBABABBBBBBBBBBBBBBBBBBBBBBBBBBBABABBBABABBBBBBBBBABABBBABA
