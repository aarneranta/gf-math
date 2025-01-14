-- basic types

data Sigma (a : Set) (b : a -> Set) : Set where
  pair : (x : a) -> (y : b x) -> Sigma a b

data Pi (a : Set) (b : a -> Set) : Set where
  lambda : ((x : a) -> b x) -> Pi a b

data Disj (a b : Set) : Set where
  left : a -> Disj a b
--  right : b -> Disj a b

data False : Set where

--data Eq {a : Set} : a -> a -> Set where
  --refl : (x : a) -> Eq x x

-- defined types

Conj : (a b : Set) -> Set
--Conj a b = Sigma a (\_ -> b)

Impl : (a b : Set) -> Set
--Impl a b = Pi a (\_ -> b)

Neg : (a : Set) -> Set
--Neg a = Impl a False


-- elimination constants

fst : {a : Set} -> {b : a -> Set} -> Sigma a b -> a
fst (pair a b) = a

snd : {a : Set} -> {b : a -> Set} -> (c : Sigma a b) -> b (fst c) 
snd (pair a b) = b

app : {a : Set} -> {b : a -> Set} -> Pi a b -> (a : a) -> b a 
app (lambda f) a = f a

when : {a b c : Set} -> Disj a b -> (a -> c) -> (b -> c) -> c
when (left a) f _ = f a
when (right b) _ g = g b

postulate exfalso : {c : Set} -> False -> c --- no empty definition


