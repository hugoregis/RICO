% To run do the following:
%       a. consult this file
%       b. aleph:read_all(animals).
%       c. induce.
:- use_module(library(aleph)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(prolog).
:- endif.
:- aleph.
:- set_random(seed(111)).
:- aleph_set(evalfn,posonly).
:- aleph_set(gsamplesize,20).
:- style_check(-discontiguous).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,homeothermic(+animal)).
:- modeb(1,animal(+animal)).
:- modeb(1,class(+animal)).
:- modeb(1,class(+animal,#class)).
:- modeb(1,has_milk(+animal)).
:- modeb(1,lay_eggs(+animal)).
:- modeb(1,has_gills(+animal)).
:- modeb(1,lives_in_water(+animal)).
:- modeb(1,fly(+animal)).


:- determination(homeothermic/1,animal/1).
:- determination(homeothermic/1,class/1).
:- determination(homeothermic/1,class/2).
:- determination(homeothermic/1,has_milk/1).
:- determination(homeothermic/1,lay_eggs/1).
:- determination(homeothermic/1,has_gills/1).
:- determination(homeothermic/1,lives_in_water/1).
:- determination(homeothermic/1,fly/1).


:- dynamic(animal/1).
:- dynamic(class/1).
:- dynamic(class/2).
:- dynamic(has_milk/1).
:- dynamic(lay_eggs/1).
:- dynamic(homeothermic/1).
:- dynamic(has_gills/1).
:- dynamic(lives_in_water/1).
:- dynamic(fly/1).


:- begin_bg.

%%animal
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

%%class
class(mammal).
class(fish).
class(reptile).
class(bird).

%%class
class(eagle, bird).
class(robin, bird).
class(woodpecker, bird).
class(dog, mammal).
class(bat, mammal).
class(hawk, bird).
class(shark, fish).
class(crocodile, reptile).
class(snake, reptile).
class(platypus, mammal).
class(t_rex, reptile).
class(ostrich, bird).
class(penguin, bird).
class(trout, fish).
class(herring, fish).
class(eel, fish).
class(lizard, reptile).
class(turtle, reptile).
class(cat, mammal).
class(man, mammal).
class(dolphin, mammal).

%%has_milk
has_milk(dog).
has_milk(cat).
has_milk(dolphin).
has_milk(bat).
has_milk(platypus).
has_milk(man).

%%lay_eggs
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

%%has_gills
has_gills(trout).
has_gills(herring).
has_gills(shark).
has_gills(eel).

%%lives_in_water
lives_in_water(dolphin).
lives_in_water(platypus).
lives_in_water(trout).
lives_in_water(herring).
lives_in_water(shark).
lives_in_water(eel).

%%fly
fly(robin).
fly(woodpecker).
fly(eagle).
fly(hawk).
fly(bat).

:-end_bg.


:-begin_in_pos.

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

:-end_in_pos.
