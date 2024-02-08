/- https://avigad.github.io/lamr/using_lean_as_a_programming_language.html -/

def four : Nat := 2 + 2

def isOne (x : Nat) : String := if x = 1 then "yes" else "no"

def four' := 2 + 2

def isOne' x := if x = 1 then "yes" else "no"

def Fermat_statement : Prop :=
∀ a b c n : Nat, a * b * c ≠ 0 ∧ n > 2 → a^n + b^n ≠ c^n

theorem two_plus_two_is_four : 2 + 2 = 4 := rfl

theorem Fermat_last_theorem : Fermat_statement := sorry

def foo n := 3 * n + 7

def bar n := foo (foo n) + 3

/- parsed 240207 although not properly -/
def printExample : IO Unit:= do
  IO.println "hello"
  IO.println "world"

def factorial : Nat → Nat
  | 0       => 1
  | (n + 1) => (n + 1) * factorial n

def hanoi (numDisks start finish aux : Nat) : IO Unit :=
  match numDisks with
  | 0     => pure ()
  | n + 1 => do
      hanoi n start aux finish
      IO.println s!"Move disk {n + 1} from peg {start} to peg {finish}"
      hanoi n aux finish start

def addNums : List Nat → Nat
  | []    => 0
  | a::as => a + addNums as

open List

def myRange := List.range 7

end

namespace hidden

def reverseAux : List α → List α → List α
  | [],   r => r
  | a::l, r => reverseAux l (a::r)

def reverse (as : List α) :List α :=
  reverseAux as []

protected def append (as bs : List α) : List α :=
  reverseAux as.reverse bs

end hidden

partial def gcd m n :=
  if n = 0 then m else gcd n (m % n)

partial def bad (n : Nat) : Nat := bad (n + 1)

def fib' : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib' (n + 1) + fib' n

def fibAux : Nat → Nat × Nat
  | 0     => (0, 1)
  | n + 1 => let p := fibAux n
             (p.2, p.1 + p.2)

def fib n := (fibAux n).1


import Init

inductive BinTree
  | empty : BinTree
  | node  : BinTree → BinTree → BinTree
  deriving Repr, DecidableEq, Inhabited

open BinTree

def size : BinTree → Nat
  | empty    => 0
  | node a b => 1 + size a + size b

def depth : BinTree → Nat
  | empty    => 0
  | node a b => 1 + Nat.max (depth a) (depth b)

def example_tree := node (node empty empty) (node empty (node empty empty))

def foo (b : BinTree) : Nat :=
  match b with
  | empty    => 0
  | node _ _ => 1

def bar (n? : Option Nat) : Nat :=
  match n? with
  | some n => n
  | none   => 0


