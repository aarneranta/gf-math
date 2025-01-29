module BaseConstants where
 --postulate Set : Set

  postulate Prop : Set
  

  Elem : Set -> Set
  Elem A = A

  postulate Proof : Prop -> Set

  postulate false : Prop
  postulate and : (A : Prop) -> (B : Prop) -> Prop
  postulate or : (A : Prop) -> (B : Prop) -> Prop
  postulate if : Prop -> Prop -> Prop
  postulate foral : (A : Set) -> (B : (Elem A -> Prop)) -> Prop
  postulate exists : (A : Set) -> (B : (Elem A -> Prop)) -> Prop

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

  postulate nd : Elem Dig -> Elem Nat
  postulate nn : Elem Dig -> Elem Nat -> Elem Nat

  postulate Eq : (x : Elem Real) -> (y : Elem Real) -> Prop
  postulate Lt : Elem Real -> Elem Real -> Prop

  postulate Gt : Elem Real -> Elem Real -> Prop
  postulate Neq : Elem Real -> Elem Real -> Prop

  postulate Leq : Elem Real -> Elem Real -> Prop
  postulate Geq : Elem Real -> Elem Real -> Prop

  postulate positive : (x : Elem Real) -> Prop
  postulate negative : Elem Real -> Prop

  postulate plus : (x : Elem Real) -> (y : Elem Real) -> Elem Real
  postulate minus : Elem Real -> Elem Real -> Elem Real
  postulate times : Elem Real -> Elem Real -> Elem Real
  postulate div : Elem Real -> Elem Real -> Elem Real
  postulate pow : Elem Real -> Elem Real -> Elem Real
  postulate gcd : Elem Int -> Elem Int -> Elem Int
  postulate factorial : Elem Nat -> Elem Nat

  postulate even : Elem Int -> Prop

  odd : Elem Int -> Prop
  odd = \ n -> not (even n)

  divisible : Elem Int -> Elem Int -> Prop
  divisible = \ n -> \ m -> exists Int (\ k -> Eq n (times k m))

  prime : Elem Nat -> Prop
  prime = \ n -> not (exists Nat (\ m -> and (Lt 1 m) (and (Lt m n) (divisible n m))))

  postulate function : Set -> Set -> Set
  postulate union : Set -> Set -> Set
  postulate intersection : Set -> Set -> Set
  postulate difference : Set -> Set -> Set
  postulate powerset : Set -> Set
  postulate suchthat : (A : Set) -> ((B : Elem A) -> Prop) -> Set
  postulate fst : (A : Set) -> (B : Elem A -> Prop) -> Elem (suchthat A B) -> Elem A

