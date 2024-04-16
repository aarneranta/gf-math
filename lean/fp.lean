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


-- 1.6

structure PPPoint (α : Type) where
  x : α
  y : α
 deriving Repr

def natOrigin : PPPoint Nat :=
  { x := Nat.zero, y := Nat.zero }

def replaceX (α : Type) (point : PPPoint α) (newX : α) : PPPoint α :=
  { point with x := newX }

#check (replaceX)
#check replaceX Nat

#eval replaceX Nat natOrigin 5

inductive Sign where
  | pos
  | neg

def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)


-- 1.6

def primesUnder10 : List Nat := [2, 3, 5, 7]

inductive LList (α : Type) where
  | nil : LList α
  | cons : α → LList α → LList α

def explicitPrimesUnder10 : List Nat :=
  List.cons 2 (List.cons 3 (List.cons 5 (List.cons 7 List.nil)))

def length (α : Type) (xs : List α) : Nat :=
  match xs with
  | List.nil => Nat.zero
  | List.cons y ys => Nat.succ (length α ys)


def llength (α : Type) (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (llength α ys)

def rreplaceX {α : Type} (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX } --- error: 'sorryAx' is not a structure

#eval rreplaceX natOrigin 5

def lllength {α : Type} (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (lllength ys)

#eval lllength primesUnder10

#check List.length (α := Int)

inductive OOption (α : Type) : Type where
  | none : OOption α
  | some (val : α) : OOption α


def List.hhead? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => none
  | y :: _ => some y

#eval primesUnder10.hhead?
#eval [].hhead? (α := Int)
#eval ([] : List Int).hhead?

structure PPProd (α : Type) (β : Type) : Type where
  fst : α
  snd : β

def fives : String × Int := { fst := "five", snd := 5 }
def ffives : String × Int := ("five", 5)

inductive SSum (α : Type) (β : Type) : Type where
  | inl : α → SSum α β
  | inr : β → SSum α β

def PetName : Type := String ⊕ String

def animals : List PetName :=
  [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi", Sum.inl "Rex", Sum.inr "Floof"]

def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | Sum.inl _ :: morePets => howManyDogs morePets + 1
  | Sum.inr _ :: morePets => howManyDogs morePets

inductive UUnit : Type where
  | unit : UUnit

inductive ArithExpr (ann : Type) : Type where
  | int : ann → Int → ArithExpr ann
  | plus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | minus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | times : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann

#eval (() : Unit)


-- 1.7

def lllength {α : Type} (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (lllength ys)

def llllength (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (llllength ys)

def lllllength : List α → Nat
  | [] => 0
  | y :: ys => Nat.succ (lllllength ys)

def drop : Nat → List α → List α
  | Nat.zero, xs => xs
  | _, [] => []
  | Nat.succ n, x :: xs => drop n xs


def fromOption (default : α) : Option α → α
  | none => default
  | some x => x

#eval (some "salmonberry").getD ""

#eval none.getD ""

def unzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    (x :: (unzip xys).fst, y :: (unzip xys).snd)

def uunzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped : List α × List β := uunzip xys
    (x :: unzipped.fst, y :: unzipped.snd)
-- To use let on a single line, separate the local definition from the body with a semicolon.

def uuunzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let (xs, ys) : List α × List β := uuunzip xys
    (x :: xs, y :: ys)

def reverse (xs : List α) : List α :=
  let rec helper : List α → List α → List α
    | [], soFar => soFar
    | y :: ys, soFar => helper ys (y :: soFar)
  helper xs []

def uuuunzip (pairs : List (α × β)) :=
  match pairs with
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped := uuuunzip xys
    (x :: unzipped.fst, y :: unzipped.snd)

def ddrop (n : Nat) (xs : List α) : List α :=
  match n, xs with
  | Nat.zero, ys => ys
  | _, [] => []
  | Nat.succ n , y :: ys => ddrop n ys

def eeven : Nat → Bool
  | 0 => true
  | n + 1 => not (eeven n)

def halve : Nat → Nat
  | 0 => 0
  | 1 => 0
  | n + 2 => halve n + 1

#check fun x => x + 1

#check fun {α : Type} (x : α) => x
#check λ {α : Type} (x : α) => x

#check fun
  | 0 => none
  | n + 1 => some n

def double : Nat → Nat := fun
  | 0 => 0
  | k + 1 => double k + 2

#eval (· * 2) 5

def Nat.double (x : Nat) : Nat := x + x

#eval (4 : Nat).double

namespace NewNamespace
def triple (x : Nat) : Nat := 3 * x
def quadruple (x : Nat) : Nat := 2 * x + 2 * x
end NewNamespace

#check NewNamespace.triple
#check NewNamespace.quadruple

def timesTwelve (x : Nat) :=
  open NewNamespace in
  quadruple (triple x)

open NewNamespace in
  #check quadruple

inductive Inline : Type where
  | lineBreak
  | string : String → Inline
  | emph : Inline → Inline
  | strong : Inline → Inline

def Inline.string? (inline : Inline) : Option String :=
  match inline with
  | Inline.string s => some s
  | _ => none

def Inline.sstring? (inline : Inline) : Option String :=
  if let Inline.string s := inline then
    some s
  else none

#eval (⟨1, 2⟩ : Point)

#eval s!"three fives is {NewNamespace.triple 5}"

  