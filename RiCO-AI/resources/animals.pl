:- module(animals, [animal/1, class/1, class/2, has_milk/1, lay_eggs/1, homeothermic/1, has_gills/1, lives_in_water/1, fly/1]).

animal(dog).  
animal(dolphin).  
animal(platypus).  
animal(bat).
animal(trout).  
animal(herring).  
animal(shark). 
animal(eel).
animal(lizard).  
animal(crocodile).  
animal(t_rex).  
animal(turtle).
animal(snake).  
animal(eagle).  
animal(ostrich).  
animal(penguin).
animal(cat). 
animal(dragon).  
animal(man).  
animal(woodpecker).
animal(robin).

class(mammal).  
class(fish).  
class(reptile).  
class(bird).

class(eagle,bird).
class(robin,bird).
class(woodpecker,bird).
class(dog,mammal).
class(bat,mammal).
class(hawk,bird).
class(shark,fish).
class(crocodile,reptile).
class(snake,reptile).
class(platypus,mammal).
class(t_rex,reptile).
class(ostrich,bird).
class(penguin,bird).
class(trout,fish).
class(herring,fish).
class(eel,fish).
class(lizard,reptile).
class(turtle,reptile).
class(cat,mammal).
class(man,mammal).
class(dolphin,mammal).

has_milk(dog).
has_milk(cat).
has_milk(dolphin).
has_milk(bat).
has_milk(platypus).
has_milk(man).

lay_eggs(platypus).
lay_eggs(trout).
lay_eggs(herring).
lay_eggs(shark).
lay_eggs(eel).
lay_eggs(lizard).
lay_eggs(crocodile).
lay_eggs(t_rex).
lay_eggs(snake).
lay_eggs(turtle).
lay_eggs(eagle).
lay_eggs(ostrich).
lay_eggs(penguin).
lay_eggs(hawk).
lay_eggs(robin).
lay_eggs(woodpecker).

homeothermic(dog).
homeothermic(cat).
homeothermic(dolphin).
homeothermic(platypus).
homeothermic(bat).
homeothermic(eagle).
homeothermic(robin).
homeothermic(woodpecker).
homeothermic(hawk).
homeothermic(ostrich).
homeothermic(penguin).

has_gills(trout).
has_gills(herring).
has_gills(shark).
has_gills(eel).

lives_in_water(dolphin).
lives_in_water(platypus).
lives_in_water(trout).
lives_in_water(herring).
lives_in_water(shark).
lives_in_water(eel).

fly(robin).
fly(woodpecker).
fly(eagle).
fly(hawk).
fly(bat).
