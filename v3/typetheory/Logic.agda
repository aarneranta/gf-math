-- basic types

postulate Sigma : (a : Set) -> (b : a -> Set) -> Set 
postulate pair : (a : Set) -> (b : a -> Set) -> (x : a) -> (y : b x) -> Sigma a b

postulate Pi : (a : Set) -> (b : a -> Set) -> Set
postulate lambda : (a : Set) -> (b : a -> Set) -> ((x : a) -> b x) -> Pi a b

postulate Disj : (a b : Set) -> Set
postulate left : (a b : Set) -> a -> Disj a b
postulate right : (a b : Set) -> b -> Disj a b

postulate False : Set

postulate Eq : (a : Set) -> a -> a -> Set
postulate refl : (a : Set) -> (x : a) -> Eq a x x

-- defined types

Conj : (a b : Set) -> Set
Conj = \a b -> Sigma a (\_ -> b)

Impl : (a b : Set) -> Set
Impl = \a b -> Pi a (\_ -> b)

Neg : (a : Set) -> Set
Neg = \a -> Impl a False


-- elimination constants

postulate fst : (a : Set) -> (b : a -> Set) -> Sigma a b -> a
-- fst _ _ (pair _ _ a b) = a

--- postulate snd : (a : Set) -> (b : a -> Set) -> (c : Sigma a b) -> b (fst c) 
-- snd _ _ (pair _ _ a b) = b

postulate app : (a : Set) -> (b : a -> Set) -> Pi a b -> (a : a) -> b a 
-- app _ _ (lambda _ _ f) a = f a

postulate when : (a b c : Set) -> Disj a b -> (a -> c) -> (b -> c) -> c
-- when _ _ _ (left _ _ a) f _ = f a
-- when _ _ _ (right _ _ b) _ g = g b

postulate exfalso : (c : Set) -> False -> c


