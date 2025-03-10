module BaseConstants where

--  Prop = Set --- depends on Agda version

  data false : Prop where
  
  data and (A B : Prop) : Prop where
    andI : A -> B -> and A B
    
  data or (A B : Prop) : Prop where
    orIL : A -> or A B
    orIR : B -> or A B
    
  if : Prop -> Prop -> Prop
  if A B = A -> B
  
  all : (A : Set) -> (B : (A -> Prop)) -> Prop
  all A B = (x : A) -> B x
  
  data exists (A : Set) (B : (A -> Prop)) : Prop where
    existI : (a : A) -> B a -> exists A B

  not : Prop -> Prop
  not = \ A -> if A false

  iff : Prop -> Prop -> Prop
  iff = \ A -> \ B -> and (if A B) (if B A)

  data Num : Set where
    Zero : Num
    Succ : Num -> Num

  {-# BUILTIN NATURAL Num #-}

  Dig : Set
  Dig = Num
  
  Nat : Set
  Nat = Num

  Int : Set
  Int = Num

  Rat : Set
  Rat = Num

  Real : Set
  Real = Num
 
  Complex : Set
  Complex = Num

  nd : Dig -> Nat
  nd x = x
  
  postulate nn : Dig -> Nat -> Nat

  postulate Eq : (x : Real) -> (y : Real) -> Prop
  postulate Lt : Real -> Real -> Prop

  postulate Gt : Real -> Real -> Prop
  postulate Neq : Real -> Real -> Prop

  postulate Leq : Real -> Real -> Prop
  postulate Geq : Real -> Real -> Prop

  postulate positive : (x : Real) -> Prop
  postulate negative : Real -> Prop

  postulate plus : (x : Real) -> (y : Real) -> Real
  postulate minus : Real -> Real -> Real
  postulate times : Real -> Real -> Real
  postulate div : Real -> Real -> Real
  postulate pow : Real -> Real -> Real
  postulate gcd : Int -> Int -> Int
  postulate factorial : Nat -> Nat

  postulate even : Int -> Prop

  odd : Int -> Prop
  odd = \ n -> not (even n)

  divisible : Int -> Int -> Prop
  divisible = \ n -> \ m -> exists Int (\ k -> Eq n (times k m))

  prime : Nat -> Prop
  prime = \ n -> not (exists Nat (\ m -> and (Lt 1 m) (and (Lt m n) (divisible n m))))

  postulate function : Set -> Set -> Set
  postulate union : Set -> Set -> Set
  postulate intersection : Set -> Set -> Set
  postulate difference : Set -> Set -> Set
  postulate powerset : Set -> Set
  postulate suchthat : (A : Set) -> ((B : A) -> Prop) -> Set
  postulate fst : (A : Set) -> (B : A -> Prop) -> (suchthat A B) -> A

