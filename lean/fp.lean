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

structure Point where
    x : Float
    y : Float
  deriving Repr  --- must be indented for bnfc

def origin : Point := { x := 0.0, y := 0.0 }

#eval origin
#eval origin.x

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt (((p2.x - p1.x) ^ 2.0) + ((p2.y - p1.y) ^ 2.0))

#eval distance { x := 1.0, y := 2.0 } { x := 5.0, y := -1.0 }

structure Point3D where
  x : Float
  y : Float
  z : Float
 deriving Repr

def origin3D : Point3D := { x := 0.0, y := 0.0, z := 0.0 }

#check ({ x := 0.0, y := 0.0 } : Point) --- Lean does not infer principal type of struct

#check { x := 0.0, y := 0.0 : Point}

def zeroX (p : Point) : Point :=
  { p with x := 0 }

def fourAndThree : Point :=
  { x := 4.3, y := 3.4 }
#eval fourAndThree
#eval zeroX fourAndThree

#check Point.mk 1.5 2.8 -- constructor of Point

structure PPoint where
  point ::
  x : Float
  y : Float
 deriving Repr

#eval "one string".append " and another"

def Point.modifyBoth (f : Float → Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

#eval fourAndThree.modifyBoth Float.floor


-- 1.5

inductive BBool where
  | false : BBool  --- layout syntax really not required, because of |
  | true : BBool

inductive NNat where
  | zero : NNat
  | succ (n : NNat) : NNat


def isZero (n : Nat) : Bool :=
  match n with  --- should this have a layout syntax ?
  | Nat.zero => true
  | Nat.succ _ => false --- warning: unused variable (if k as in the tutorial)

#eval isZero (Nat.succ Nat.zero)

def depth (p : Point3D) : Float :=
  match p with
  | { x:= h, y := w, z := d } => d --- unused variables here as well

def even (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (even k)

--- lean has rather aggressive termination checking, requiring primitive recursion it seems

def plus (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => Nat.succ (plus n k')


