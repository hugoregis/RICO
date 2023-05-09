-- 1) Après importation de la base de connaissance brute

aBird = tau (Being Bird)
myNewBird = delta (Being Lay_Eggs) aBird
predicate (Being Bird) myNewBird
predicate (Being Fly) aBird
predicate (Being Fly) myNewBird


-- 2) Après importation ILP

aBird = tau (Being Bird)
myNewBird = delta (Being Lay_Eggs) aBird
predicate (Being Bird) myNewBird
predicate (Being Fly) aBird
predicate (Being Fly) myNewBird

-- => Comportement attendu : myNewBird flies

anOstrich = tau (Being Ostrich)
aHawk = tau (Being Hawk)
predicate (Being Bird) anOstrich
predicate (Being Bird) aHawk
predicate (Being Fly) aHawk
predicate (NotBeing Fly) aHawk
predicate (Being Fly) anOstrich
predicate (NotBeing Fly) anOstrich
predicate (Being Fly) aBird
typical (Being Bird) anOstrich
typical (Being Bird) aHawk

-- => Comme avec Prolog, on n'a pas une prise en compte satisfaisante des autruches

-- 3) Après rectification sur les autruches

aBird = tau (Being Bird)
anOstrich = tau (Being Ostrich)
aHawk = tau (Being Hawk)
predicate (Being Bird) anOstrich
predicate (Being Bird) aHawk
predicate (Being Fly) aHawk
predicate (NotBeing Fly) aHawk
predicate (Being Fly) anOstrich
predicate (NotBeing Fly) anOstrich
predicate (Being Fly) aBird
typical (Being Bird) anOstrich
typical (Being Bird) aHawk

-- => On a bien le comportement logique (naturel) attendu qu'on ne parvenait pas à obtenir avec Prolog.

-- On peut même concevoir une autruche atypique...

rita = delta (Being Fly) anOstrich
typical (Being Ostrich) rita
typical (Being Bird) rita
bob = delta (NotBeing Fly) aHawk
typical (Being Hawk) bob
typical (Being Bird) bob









-- "Monde ouvert: pas de tiers-exclus.
-- même problème que sous Prolog



aFlyingBird = delta (Being Fly) aBird
anOstrichBird = delta (Being Ostrich) aBird
aNonFlyingHawk = delta (NotBeing Fly) aHawk
aNonFlyingOstrich = delta (NotBeing Fly) anOstrich
aNonFlyingBird = delta (NotBeing Fly) aBird
aFlyingOstrich = delta (Being Fly) anOstrich
