def false : Prop := False
-- def And (A : Prop)(B : Prop) : Prop := And A B
-- def Or (A : Prop)(B : Prop) : Prop := Or A B
def If (A B : Prop) : Prop := A -> B
def All (A : Type)(B : A -> Prop) : Prop := forall x : A, (B x)
def Exist (A : Type)(B : A -> Prop) : Prop := exists x : A, (B x)
-- def Not (A : Prop) : Prop := Not A
-- def Iff (A B : Prop) : Prop := Iff A B


def Num : Type := Int

def Dig : Type := Num
--def Nat : Set := Num
--def Int : Type := Num
def Rat : Type := Num
def Real : Type := Num
def Complex : Type := Num


-- basic comparison relations
--def Eq (x : Real) (y : Real) : Prop := Eq x y
def Lt (x : Int) (y : Int) : Prop := x < y
def Gt (x : Int) (y : Int) : Prop := x > y
def Neq (x : Int) (y : Int) : Prop := Not (x = y)
def Leq (x : Int) (y : Int) : Prop := Not (x > y)
def Geq (x : Int) (y : Int) : Prop := Not (x < y)

def positive (x : Int) : Prop := x > 0
def negative (x : Int) : Prop := x < 0

def plus (x y : Int) : Int := x + y
def minus (x y : Int) : Int := x - y
def times (x y : Int) : Int := x * y
def div (x y : Int) : Int := x / y
---def pow (x y : Int) : Int := x ^ y  --- failed to synthesize instance HPow Int Int ?m.392
def gcd (x y : Int) : Int := sorry ---
def factorial (x : Int) : Int := sorry ---

def even (n : Int) : Prop := exists m : Int, n = 2*m
def odd (n : Int) : Prop := Not (even n)

def divisible (n m : Int) : Prop := exists x : Int, n = x * m

def prime (n : Nat) : Prop := Not (exists m : Nat, And (1 < m) (And (m < n) (divisible n  m)))

/-
(; set operations ;)
function : Set -> Set -> Set.
union : Set -> Set -> Set.
intersection : Set -> Set -> Set.
difference : Set -> Set -> Set.
powerset : Set -> Set.

(; hard-coded in MathCore ;)
suchthat : (A : Set) -> (B : Elem A -> Prop) -> Set.
fst : (A : Set) -> (B : (Elem A -> Prop)) -> Elem (suchthat A B) -> Elem A.
-/
