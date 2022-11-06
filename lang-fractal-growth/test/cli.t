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
  monopodial-inflorence

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
  B(2) A(4, 4)
  B(1) B(4) A(1, 0)
  B(0) B(3) A(2, 1)
  C B(2) A(4, 3)
  C B(1) A(8, 7)
  C B(0) B(8) A(1, 0)
  C C B(7) A(2, 1)
  C C B(6) A(4, 3)
  C C B(5) A(8, 7)
  C C B(4) B(8) A(1, 0)
  C C B(3) B(7) A(2, 1)
  C C B(2) B(6) A(4, 3)
  C C B(1) B(5) A(8, 7)
  C C B(0) B(4) B(8) A(1, 0)
  C C C B(3) B(7) A(2, 1)
  C C C B(2) B(6) A(4, 3)

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

Monopodial inflorence
  $ fractal-growth generations --system=monopodial-inflorence | head --lines=8
  A
  I[A]A
  I[B]I[A]A
  I[C]I[B]I[A]A
  I[C]I[C]I[B]I[A]A
  I[C]I[C]I[C]I[B]I[A]A
  I[C]I[C]I[C]I[C]I[B]I[A]A
  I[C]I[C]I[C]I[C]I[C]I[B]I[A]A
