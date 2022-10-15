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
  parametric
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

Parametric L-System
  $ fractal-growth generations --system=parametric | head --lines=16
  B(2)A(4, 4)
  B(1)B(4)A(1, 0)
  B(0)B(3)A(2, 1)
  CB(2)A(4, 3)
  CB(1)A(8, 7)
  CB(0)B(8)A(1, 0)
  CCB(7)A(2, 1)
  CCB(6)A(4, 3)
  CCB(5)A(8, 7)
  CCB(4)B(8)A(1, 0)
  CCB(3)B(7)A(2, 1)
  CCB(2)B(6)A(4, 3)
  CCB(1)B(5)A(8, 7)
  CCB(0)B(4)B(8)A(1, 0)
  CCCB(3)B(7)A(2, 1)
  CCCB(2)B(6)A(4, 3)

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
