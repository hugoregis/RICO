module KB2 where

-- import LOD_Basics

data Name = Thing | Animal | Reptile | Fish | Mammal | Has_Milk | Homeothermic | Has_Hair | Bird | Fly | Lay_Eggs | Has_Gills | Has_Scales | Lives_in_water | Has_Feather | Dog | Eagle | Robin | Woodpecker | Bat | Hawk | Shark | Crocodile | Snake | Platypus | T_rex | Ostrich | Penguin | Trout | Herring | Eel | Lizard | Turtle | Cat | Man | Dolphin deriving (Eq, Ord, Show, Read, Enum, Bounded) 


categ :: Name -> ([Name],[Name])
-- Base de donnée logique initiale (convertie depuis Prolog)
categ Animal = ([Thing],[])
categ Dog = ([Animal, Mammal, Has_Milk, Homeothermic, Has_Hair],[])
categ Eagle = ([Animal, Bird, Lay_Eggs, Homeothermic, Fly],[])
categ Robin = ([Animal, Bird, Lay_Eggs, Homeothermic, Fly],[])
categ Woodpecker = ([Animal, Bird, Lay_Eggs, Homeothermic, Fly],[])
categ Bat = ([Animal, Mammal, Has_Milk, Homeothermic, Has_Hair, Fly],[])
categ Hawk = ([Animal, Bird, Lay_Eggs, Homeothermic, Fly],[])
categ Shark = ([Animal, Fish, Lay_Eggs, Has_Gills],[])
categ Crocodile = ([Animal, Reptile, Lay_Eggs],[])
categ Snake = ([Animal, Reptile, Lay_Eggs],[])
categ Platypus = ([Animal, Mammal, Has_Milk, Lay_Eggs, Homeothermic, Has_Hair],[])
categ T_rex = ([Animal, Reptile, Lay_Eggs],[])
categ Ostrich = ([Animal, Bird, Lay_Eggs, Homeothermic],[]) -- Add Not Fly : rectification post-apprentissage
categ Penguin = ([Animal, Bird, Lay_Eggs, Homeothermic],[])
categ Trout = ([Animal, Fish, Lay_Eggs, Has_Gills, Has_Scales],[])
categ Herring = ([Animal, Fish, Lay_Eggs, Has_Gills],[])
categ Eel = ([Animal, Fish, Lay_Eggs, Has_Gills],[])
categ Lizard = ([Animal, Reptile, Lay_Eggs],[])
categ Turtle = ([Animal, Reptile, Lay_Eggs],[])
categ Cat = ([Animal, Mammal, Has_Milk, Homeothermic],[])
categ Man = ([Animal, Mammal, Has_Milk, Homeothermic],[])
categ Dolphin = ([Animal, Mammal, Has_Milk, Homeothermic],[])
-- Base de donnée logique apprise (convertie depuis Aleph/Prolog)
categ Bird = ([Animal, Fly, Homeothermic],[]) -- ILP learned
categ Fish = ([Animal, Has_Gills, Homeothermic],[]) -- ILP learned
categ Lay_Eggs = ([Animal],[]) -- ILP learned
categ Lives_in_water = ([Animal],[]) -- ILP learned
categ Mammal = ([Animal, Homeothermic, Has_Milk],[]) -- ILP learned
categ x = ([],[])

categEss :: Name -> [Name]
categEss Animal = [Thing]
categEss Dog = [Animal]
categEss Eagle = [Animal]
categEss Robin = [Animal]
categEss Bird = [Animal, Fly, Homeothermic] -- remove Fly
categEss x = []


-- aBird = tau (Being Bird)
-- aRedBird = delta (Being Red) aBird
-- anAnimal = tau (Being Animal)
-- aOstrichBird = delta (Being Ostrich) aBird
-- aGreenCanaryAnimal = deltaChain [Being Green, Being Canary] anAnimal
-- rita = deltaChain [(Being Flying), (Being Yellow), (NotBeing InTheHouse)] $ tau (Being Ostrich)
-- julie = deltaChain [(NotBeing Colorful), (NotBeing InTheHouse)] $ tau (Being Ostrich)
