Genealogy example
  $ cat ./genealogy.datalog | datalog
  ────────────────────────────────────────────────────────────────────────────────
  Knowledge Base
  ────────────────────────────────────────────────────────────────────────────────
  parent("Arwen", "Eldarion").
  parent("Aragorn II", "Eldarion").
  parent("Celebrían", "Elladan").
  parent("Celebrían", "Elrohir").
  parent("Celebrían", "Arwen").
  parent("Elrond", "Elladan").
  parent("Elrond", "Elrohir").
  parent("Elrond", "Arwen").
  parent("Eärwen", "Galadriel").
  parent("Finarfen", "Galadriel").
  parent("Elwing", "Elrond").
  parent("Elwing", "Elros").
  parent("Eärendil", "Elrond").
  parent("Eärendil", "Elros").
  parent("Galadhon", "Celeborn").
  parent("Galadhon", "Galathil").
  parent("Galadriel", "Celebrían").
  parent("Celeborn", "Celebrían").
  parent("Galathil", "Nimloth").
  parent("Lúthien", "Dior").
  parent("Beren", "Dior").
  parent("Melian", "Lúthien").
  parent("Elu Thingol", "Lúthien").
  parent("Nimloth", "Elwing").
  parent("Dior", "Elwing").
  parent("Tuor", "Eärendil").
  parent("Idril", "Eärendil").
  ancestor("Arwen", "Eldarion").
  ancestor("Aragorn II", "Eldarion").
  ancestor("Celebrían", "Elladan").
  ancestor("Celebrían", "Elrohir").
  ancestor("Celebrían", "Arwen").
  ancestor("Elrond", "Elladan").
  ancestor("Elrond", "Elrohir").
  ancestor("Elrond", "Arwen").
  ancestor("Eärwen", "Galadriel").
  ancestor("Finarfen", "Galadriel").
  ancestor("Elwing", "Elrond").
  ancestor("Elwing", "Elros").
  ancestor("Eärendil", "Elrond").
  ancestor("Eärendil", "Elros").
  ancestor("Galadhon", "Celeborn").
  ancestor("Galadhon", "Galathil").
  ancestor("Galadriel", "Celebrían").
  ancestor("Celeborn", "Celebrían").
  ancestor("Galathil", "Nimloth").
  ancestor("Lúthien", "Dior").
  ancestor("Beren", "Dior").
  ancestor("Melian", "Lúthien").
  ancestor("Elu Thingol", "Lúthien").
  ancestor("Nimloth", "Elwing").
  ancestor("Dior", "Elwing").
  ancestor("Tuor", "Eärendil").
  ancestor("Idril", "Eärendil").
  ancestor("Elros", "Eldarion").
  ancestor("Elwing", "Aragorn II").
  ancestor("Eärendil", "Aragorn II").
  ancestor("Celebrían", "Eldarion").
  ancestor("Elrond", "Eldarion").
  ancestor("Galadriel", "Elladan").
  ancestor("Celeborn", "Elladan").
  ancestor("Galadriel", "Elrohir").
  ancestor("Celeborn", "Elrohir").
  ancestor("Galadriel", "Arwen").
  ancestor("Celeborn", "Arwen").
  ancestor("Elwing", "Elladan").
  ancestor("Eärendil", "Elladan").
  ancestor("Elwing", "Elrohir").
  ancestor("Eärendil", "Elrohir").
  ancestor("Elwing", "Arwen").
  ancestor("Eärendil", "Arwen").
  ancestor("Nimloth", "Elrond").
  ancestor("Dior", "Elrond").
  ancestor("Nimloth", "Elros").
  ancestor("Dior", "Elros").
  ancestor("Tuor", "Elrond").
  ancestor("Idril", "Elrond").
  ancestor("Tuor", "Elros").
  ancestor("Idril", "Elros").
  ancestor("Eärwen", "Celebrían").
  ancestor("Finarfen", "Celebrían").
  ancestor("Galadhon", "Celebrían").
  ancestor("Galadhon", "Nimloth").
  ancestor("Melian", "Dior").
  ancestor("Elu Thingol", "Dior").
  ancestor("Galathil", "Elwing").
  ancestor("Lúthien", "Elwing").
  ancestor("Beren", "Elwing").
  ancestor("Nimloth", "Aragorn II").
  ancestor("Dior", "Aragorn II").
  ancestor("Tuor", "Aragorn II").
  ancestor("Idril", "Aragorn II").
  ancestor("Elwing", "Eldarion").
  ancestor("Eärendil", "Eldarion").
  ancestor("Galadriel", "Eldarion").
  ancestor("Celeborn", "Eldarion").
  ancestor("Eärwen", "Elladan").
  ancestor("Finarfen", "Elladan").
  ancestor("Galadhon", "Elladan").
  ancestor("Eärwen", "Elrohir").
  ancestor("Finarfen", "Elrohir").
  ancestor("Galadhon", "Elrohir").
  ancestor("Eärwen", "Arwen").
  ancestor("Finarfen", "Arwen").
  ancestor("Galadhon", "Arwen").
  ancestor("Nimloth", "Elladan").
  ancestor("Dior", "Elladan").
  ancestor("Tuor", "Elladan").
  ancestor("Idril", "Elladan").
  ancestor("Nimloth", "Elrohir").
  ancestor("Dior", "Elrohir").
  ancestor("Tuor", "Elrohir").
  ancestor("Idril", "Elrohir").
  ancestor("Nimloth", "Arwen").
  ancestor("Dior", "Arwen").
  ancestor("Tuor", "Arwen").
  ancestor("Idril", "Arwen").
  ancestor("Galathil", "Elrond").
  ancestor("Lúthien", "Elrond").
  ancestor("Beren", "Elrond").
  ancestor("Galathil", "Elros").
  ancestor("Lúthien", "Elros").
  ancestor("Beren", "Elros").
  ancestor("Galadhon", "Elwing").
  ancestor("Melian", "Elwing").
  ancestor("Elu Thingol", "Elwing").
  ancestor("Galathil", "Aragorn II").
  ancestor("Lúthien", "Aragorn II").
  ancestor("Beren", "Aragorn II").
  ancestor("Nimloth", "Eldarion").
  ancestor("Dior", "Eldarion").
  ancestor("Tuor", "Eldarion").
  ancestor("Idril", "Eldarion").
  ancestor("Eärwen", "Eldarion").
  ancestor("Finarfen", "Eldarion").
  ancestor("Galadhon", "Eldarion").
  ancestor("Galathil", "Elladan").
  ancestor("Lúthien", "Elladan").
  ancestor("Beren", "Elladan").
  ancestor("Galathil", "Elrohir").
  ancestor("Lúthien", "Elrohir").
  ancestor("Beren", "Elrohir").
  ancestor("Galathil", "Arwen").
  ancestor("Lúthien", "Arwen").
  ancestor("Beren", "Arwen").
  ancestor("Galadhon", "Elrond").
  ancestor("Melian", "Elrond").
  ancestor("Elu Thingol", "Elrond").
  ancestor("Galadhon", "Elros").
  ancestor("Melian", "Elros").
  ancestor("Elu Thingol", "Elros").
  ancestor("Galadhon", "Aragorn II").
  ancestor("Melian", "Aragorn II").
  ancestor("Elu Thingol", "Aragorn II").
  ancestor("Galathil", "Eldarion").
  ancestor("Lúthien", "Eldarion").
  ancestor("Beren", "Eldarion").
  ancestor("Melian", "Elladan").
  ancestor("Elu Thingol", "Elladan").
  ancestor("Melian", "Elrohir").
  ancestor("Elu Thingol", "Elrohir").
  ancestor("Melian", "Arwen").
  ancestor("Elu Thingol", "Arwen").
  ancestor("Melian", "Eldarion").
  ancestor("Elu Thingol", "Eldarion").
  
  ────────────────────────────────────────────────────────────────────────────────
  Query Results
  ────────────────────────────────────────────────────────────────────────────────
  ? ancestor("Elrond", "Bilbo").
    no
  
  ? ancestor("Elrond", "Eldarion").
    yes
  
  ? ancestor("Melian", X), ancestor(X, "Arwen").
    > X := "Elrond".
    > X := "Elwing".
    > X := "Dior".
    > X := "Lúthien".
    yes
  
  ? ancestor("Melian", X), ancestor(X, "Eldarion").
    > X := "Arwen".
    > X := "Aragorn II".
    > X := "Elros".
    > X := "Elrond".
    > X := "Elwing".
    > X := "Dior".
    > X := "Lúthien".
    yes
  
  ? ancestor("Galadriel", Descendent).
    > Descendent := "Celebrían".
    > Descendent := "Elladan".
    > Descendent := "Elrohir".
    > Descendent := "Arwen".
    > Descendent := "Eldarion".
    yes
  
  ? ancestor(Ancestor, Descendant), ancestor(Descendant, "Elwing").
    > Ancestor := "Galathil".
      Descendant := "Nimloth".
    > Ancestor := "Galadhon".
      Descendant := "Nimloth".
    > Ancestor := "Lúthien".
      Descendant := "Dior".
    > Ancestor := "Beren".
      Descendant := "Dior".
    > Ancestor := "Melian".
      Descendant := "Dior".
    > Ancestor := "Elu Thingol".
      Descendant := "Dior".
    > Ancestor := "Galadhon".
      Descendant := "Galathil".
    > Ancestor := "Melian".
      Descendant := "Lúthien".
    > Ancestor := "Elu Thingol".
      Descendant := "Lúthien".
    yes
  

Graph example
  $ cat ./graph.datalog | datalog
  ────────────────────────────────────────────────────────────────────────────────
  Knowledge Base
  ────────────────────────────────────────────────────────────────────────────────
  edge(1, 2).
  edge(2, 3).
  path(1, 2).
  path(2, 3).
  path(1, 3).
  
  ────────────────────────────────────────────────────────────────────────────────
  Query Results
  ────────────────────────────────────────────────────────────────────────────────
  ? path(X, 3).
    > X := 2.
    > X := 1.
    yes
  
  ? path(X, Y).
    > X := 1.
      Y := 2.
    > X := 2.
      Y := 3.
    > X := 1.
      Y := 3.
    yes
  

Zoo example
  $ cat ./zoo.datalog | datalog
  ────────────────────────────────────────────────────────────────────────────────
  Knowledge Base
  ────────────────────────────────────────────────────────────────────────────────
  size("mouse", "tiny").
  size("frog", "tiny").
  size("rabbit", "small").
  size("fox", "small").
  size("wolf", "medium").
  size("goat", "medium").
  size("pig", "medium").
  size("bear", "big").
  size("horse", "big").
  size("cow", "big").
  size("elephant", "huge").
  size("giraffe", "huge").
  colour("mouse", "grey").
  colour("mouse", "black").
  colour("mouse", "white").
  colour("frog", "green").
  colour("rabbit", "brown").
  colour("rabbit", "white").
  colour("fox", "red").
  colour("wolf", "brown").
  colour("elephant", "grey").
  feet("horse", "hooves").
  feet("cow", "hooves").
  feet("goat", "hooves").
  feet("rabbit", "paws").
  feet("fox", "paws").
  feet("bear", "paws").
  herbivore("rabbit").
  herbivore("elephant").
  herbivore("giraffe").
  carnivore("fox").
  carnivore("wolf").
  carnivore("bear").
  size("mouse", "small").
  size("frog", "small").
  size("elephant", "big").
  size("giraffe", "big").
  herbivore("horse").
  herbivore("cow").
  herbivore("goat").
  bigger("rabbit", "mouse").
  bigger("fox", "mouse").
  bigger("rabbit", "frog").
  bigger("fox", "frog").
  bigger("wolf", "rabbit").
  bigger("goat", "rabbit").
  bigger("pig", "rabbit").
  bigger("wolf", "fox").
  bigger("goat", "fox").
  bigger("pig", "fox").
  bigger("bear", "wolf").
  bigger("horse", "wolf").
  bigger("cow", "wolf").
  bigger("bear", "goat").
  bigger("horse", "goat").
  bigger("cow", "goat").
  bigger("bear", "pig").
  bigger("horse", "pig").
  bigger("cow", "pig").
  bigger("elephant", "bear").
  bigger("giraffe", "bear").
  bigger("elephant", "horse").
  bigger("giraffe", "horse").
  bigger("elephant", "cow").
  bigger("giraffe", "cow").
  eats("rabbit", "grass").
  eats("elephant", "grass").
  eats("giraffe", "grass").
  eats("rabbit", "leaves").
  eats("elephant", "leaves").
  eats("giraffe", "leaves").
  bigger("mouse", "mouse").
  bigger("frog", "mouse").
  bigger("mouse", "frog").
  bigger("frog", "frog").
  bigger("wolf", "mouse").
  bigger("goat", "mouse").
  bigger("pig", "mouse").
  bigger("wolf", "frog").
  bigger("goat", "frog").
  bigger("pig", "frog").
  bigger("elephant", "wolf").
  bigger("giraffe", "wolf").
  bigger("elephant", "goat").
  bigger("giraffe", "goat").
  bigger("elephant", "pig").
  bigger("giraffe", "pig").
  bigger("elephant", "elephant").
  bigger("giraffe", "elephant").
  bigger("elephant", "giraffe").
  bigger("giraffe", "giraffe").
  eats("horse", "grass").
  eats("cow", "grass").
  eats("goat", "grass").
  eats("horse", "leaves").
  eats("cow", "leaves").
  eats("goat", "leaves").
  eats("fox", "mouse").
  eats("fox", "frog").
  eats("wolf", "rabbit").
  eats("wolf", "fox").
  eats("bear", "wolf").
  eats("bear", "goat").
  eats("bear", "pig").
  eaten("grass").
  eaten("leaves").
  eats("wolf", "mouse").
  eats("wolf", "frog").
  eaten("mouse").
  eaten("frog").
  eaten("rabbit").
  eaten("fox").
  eaten("wolf").
  eaten("goat").
  eaten("pig").
  
  ────────────────────────────────────────────────────────────────────────────────
  Query Results
  ────────────────────────────────────────────────────────────────────────────────
  ? eats(Eater, Eaten), colour(Eater, Colour), colour(Eaten, Colour).
    > Eater := "wolf".
      Eaten := "rabbit".
      Colour := "brown".
    yes
  
  ? eaten(X), size(X, "tiny").
    > X := "mouse".
    > X := "frog".
    yes
  
  ? herbivore(Animal), size(Animal, "big"), colour(Animal, "grey").
    > Animal := "elephant".
    yes
  
