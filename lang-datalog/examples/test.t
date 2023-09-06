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
  ancestor("Galadriel", "Eldarion").
  ancestor("Celeborn", "Eldarion").
  ancestor("Elwing", "Eldarion").
  ancestor("Eärendil", "Eldarion").
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
  ancestor("Eärwen", "Eldarion").
  ancestor("Finarfen", "Eldarion").
  ancestor("Galadhon", "Eldarion").
  ancestor("Nimloth", "Eldarion").
  ancestor("Dior", "Eldarion").
  ancestor("Tuor", "Eldarion").
  ancestor("Idril", "Eldarion").
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
  ? ancestor("Melian", Intermediate), ancestor(Intermediate, "Arwen").
    > Intermediate := "Elrond".
    > Intermediate := "Elwing".
    > Intermediate := "Dior".
    > Intermediate := "Lúthien".
    Ok.
  
  ? ancestor("Elrond", "Bilbo").
    None.
  
  ? ancestor("Elrond", "Arwen").
    Ok.
  

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
