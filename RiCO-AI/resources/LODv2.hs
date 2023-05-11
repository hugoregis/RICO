{-# LANGUAGE DeriveFoldable #-}

module LODv2 where


import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.List as L

import KB2

data Concept = Being Name | NotBeing Name | BeingFromOb Object | NotBeingFromOb Object deriving (Eq, Ord, Show, Read)

data IntensionTree a = Leaf a | Node a [IntensionTree a] deriving (Eq, Show, Read, F.Foldable) 

data Det = Det {detChain :: [Concept]} deriving (Eq, Show, Read) 

data Object = Delta Det Concept | This Concept deriving (Show, Read) 

instance Eq Object where
  (==) ob1 ob2 = (intensionOb ob1) == (intensionOb ob2)

instance Ord Object where
  (>=) ob1 ob2 = S.isSubsetOf (intensionOb ob1) (intensionOb ob2)
  (<=) ob1 ob2 = S.isSubsetOf (intensionOb ob2) (intensionOb ob1)



emptyDet :: Det
emptyDet = Det {detChain = []}

ident :: Object -> Object
ident x = x

-- An empty chain of determination counts for "typical": tau = delta0. No determination = Indéterminé = typique.
tau :: Concept -> Object
tau c = Delta emptyDet c

delta :: Concept -> (Object -> Object)
delta f (Delta (Det {detChain = fs}) c) = Delta (Det {detChain = (f:fs)}) c
delta f (This c) = This c

deltaChain :: [Concept] -> (Object -> Object)
deltaChain [] = ident
deltaChain (f:fs) = delta f . (deltaChain fs)


allConcepts :: [Concept]
allConcepts = (map (Being) [(minBound :: Name) ..]) ++ (map (NotBeing) [(minBound :: Name) ..])

neg :: Concept -> Concept
neg (Being c) = NotBeing c
neg (NotBeing c) = Being c
neg (BeingFromOb c) = NotBeingFromOb c
neg (NotBeingFromOb c) = BeingFromOb c


species :: Concept -> [Concept]
species c = filter (\x -> elem c (comprehension x)) allConcepts

speciesList :: [Concept] -> [Concept]
speciesList [] = []
speciesList (c:cs) = species c ++ speciesList cs

essComprehension :: Concept -> [Concept]
essComprehension c = filter (isEssenceOf c) (comprehension c)

-- use the genus/species to find incompatibility
incompatible :: Concept -> [Concept]
incompatible (Being c) = (filter (/= Being c) (speciesList (comprehension (Being c)))) ++ [NotBeing c] ++ (concat $ map incompatible (comprehension (Being c)))
incompatible (NotBeing c) = [Being c] ++ (concat $ map incompatible (map neg $ comprehension (Being c)))

incompatible_all :: Concept -> [Concept]
incompatible_all c = filter (\x -> foldl1 (||) $ map (hasInItsIntension x) (incompatible c)) allConcepts 

compatible :: Concept -> [Concept]
compatible c = filter (\x -> not $ elem x (incompatible_all c)) allConcepts

compatibleOb :: Object -> [Concept]
compatibleOb (Delta (Det {detChain = fs}) c) = foldl1 L.intersect $ map compatible (c:fs)

incompatibleOb_all :: Object -> [Concept]
incompatibleOb_all (Delta (Det {detChain = fs}) c) = foldl1 L.intersect $ map incompatible_all (c:fs)

intension :: Concept -> IntensionTree Concept
intension c = Node c (map intension $ comprehension c)

isInIntensionOf :: Concept -> Concept -> Bool
isInIntensionOf c1 c2 = F.elem c1 (intension c2)

hasInItsIntension :: Concept -> Concept -> Bool
hasInItsIntension c1 c2 = F.elem c2 (intension c1)

properties :: Concept -> [Concept]
properties = comprehension

essence :: Concept -> IntensionTree Concept
essence c = Node c (map essence $ essComprehension c)

intensionOb :: Object -> S.Set Concept
intensionOb (Delta (Det {detChain = []}) c) = S.fromList $ F.toList (intension c)
intensionOb (Delta (Det {detChain = (f:fs)}) c) = S.union (S.fromList $ F.toList (intension f)) (intensionOb (Delta (Det {detChain = fs}) c))

isInExpansionOf :: Object -> Object -> Bool
isInExpansionOf ob1 ob2 = (ob2 >= ob1)

hasInExpansion :: Object -> Object -> Bool
hasInExpansion ob1 ob2 = (ob1 >= ob2)

generateAllObjects :: [Concept] -> S.Set Object
generateAllObjects [] = S.empty
generateAllObjects (c:cs) = S.union (S.map (\x -> deltaChain x (Delta emptyDet c)) (S.fromList (subsets $ compatible c))) (generateAllObjects cs)

expansion :: Concept -> S.Set Object
expansion c = S.filter (hasInExpansion (tau c)) (generateAllObjects [c])

expansionOb :: Object -> S.Set Object
expansionOb (Delta (Det {detChain = []}) c) = expansion c

addsIntension :: Object -> Concept -> Bool
addsIntension ob c = ((S.isSubsetOf (S.fromList $ F.toList $ intension c) (intensionOb ob)) == False) && (elem c (compatibleOb ob)) && (elem (neg c) (incompatibleOb_all ob) == False)

addsIntensionSet :: Object -> [Concept]
addsIntensionSet ob = filter (addsIntension ob) allConcepts

isFullyDeterminate :: Object -> Bool
isFullyDeterminate ob = (addsIntensionSet ob == [])

extension :: Concept -> S.Set Object
extension c = S.filter isFullyDeterminate (expansion c)

extensionOb :: Object -> S.Set Object
extensionOb (Delta (Det {detChain = []}) c) = extension c

intCharact :: Object -> [Concept]
intCharact (Delta (Det {detChain = fs}) c) = fs

complement :: [Concept] -> [Concept]
complement fs = filter (\x -> elem x fs == False) allConcepts

conceptInIntWithoutNeg :: Concept -> [Concept]
conceptInIntWithoutNeg g = (\listInt -> filter (\x -> (elem (neg x) listInt) == False) listInt) $ F.toList $ intension g

conceptInIntWithNegButCaract :: Concept -> Object -> Bool
conceptInIntWithNegButCaract g (Delta (Det det) f) =   (F.elem g (intension f)) 
                                              && (F.elem (neg g) (intension f))
                                              && (F.elem g (comprehension (BeingFromOb (Delta (Det det) f))))

-- Comprehension = Int-caract. Comprehenion f -> g modelises the intuitive notion that “the concept f directly comprises the concept g” or “the concept g is directly comprised by the concept f” . This relation is: reflexive and antisymmetric. It is not transitive.
comprehension :: Concept -> [Concept]
comprehension (Being n) = (map Being $ fst (categ n)) ++ (map NotBeing $ snd (categ n))
comprehension (NotBeing c) = []
comprehension (BeingFromOb (Delta (Det {detChain = []}) c)) = (comprehension c)
comprehension (BeingFromOb (Delta (Det {detChain = (f:fs)}) c)) = (comprehension f) ++ (comprehension (BeingFromOb (Delta (Det {detChain = (fs)}) c)))
comprehension c = [] 

isEssenceOf :: Concept -> Concept -> Bool
isEssenceOf (Being n1) (Being n2) = elem n2 $ categEss n1
isEssenceOf c1 c2 = False



inheritFrom :: Object -> [Concept]
inheritFrom (Delta (Det {detChain = (fs)}) c) = (c:fs) ++ (foldl1 (++) $ map (\y -> F.toList $ essence y) (comprehension (BeingFromOb (Delta (Det {detChain = (fs)}) c))))

predicate :: Concept -> Object -> Bool
predicate f x = (elem f (inheritFrom x)) && (elem (neg f) (intCharact x) == False)

typical :: Concept -> Object -> Bool
typical f x = foldl1 (&&) $ map (\y -> (elem (neg y) (inheritFrom x)) == False) (f:(comprehension f))

atypical :: Concept -> Object -> Bool
atypical f x = (typical f x == False)







subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

subsets :: [a] -> [[a]]
subsets l = [[]] ++ subs [[]] l
  where subs previous (x:xs) = let next = map (x:) previous
                               in next ++ subs (previous ++ next) xs
        subs _ [] = []





-- Major differences with Prolog:
--  open world
--  The program MAY be unsat (to be checked)
--  Decidable but certainly not linear complexity like SLD-Res

