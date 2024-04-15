-- https://lean-lang.org/functional_programming_in_lean/introduction.html

-- 1.1

#eval 1 + 2

#eval 1 + 2 * 5

#eval String.append "Hello, " "Lean!"

#eval String.append "it is " (if 1 > 2 then "yes" else "no")


-- 1.2

#eval (1 + 2 : Nat)

#eval (1 - 2 : Int)

#check (1 - 2 : Int)


-- 1.3

def hello := "Hello"
def lean : String := "Lean"
#eval String.append hello (String.append " " lean)

def add1 (n : Nat) : Nat := n + 1
#eval add1 7

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n
#eval maximum (5 + 8) (2 * 7)

-- these two give different output
--- but bnfc prints them in the same way
#check maximum
#check (maximum)

def Str : Type := String
def aStr : Str := "This is a string."


def NaturalNumber : Type := Nat
-- def thirtyEight : NaturalNumber := 38 -- this gives a type error in Lean
def thirtyEight : NaturalNumber := (38 : Nat)

abbrev N : Type := Nat -- reducible definition, always unfolded
def thirtyNine : N := 39
