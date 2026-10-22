# Open issues, October 2026

Left open after the work of 14-16 October 2026 (speed of generation, symbol
table checks, the autoformalization template). Each item says where it was
found and how to see it.

## Lexer and unlexer

* `lextex` and the tokens of GF's linearization disagree: GF gives `\frac{`,
  `}^{` or `x_{0}` as one token, `lextex` (which runs Haskell's `lex` on
  formulas) splits them. Since 92f49eb ranking counts GF's tokens, so this no
  longer matters there, but the two should agree.
* The unlexer leaves stray spaces: `${A }^{\complement}$`, `\frac{ m}{n}`.
  Seen in `RunInformath -variations test/sets.dk` and `test/top100.dk`.

## Speed

Generation is fast since 7e7f427, f243e24, 5416ea5 and 92f49eb (the whole of
godement-index reads back in about 2 minutes). What is left:

* `readGFTree` re-parses the GF function string of an annotated constant
  every time `Dedukti2MathCore` uses it (about 5% of the time); a fix needs
  the parsed expression carried from the symbol table.
* The rest of the time is linearization, ranking and Dedukti to MathCore.
  Profile with `stack build --profile --work-dir .stack-work-prof` and
  `+RTS -p` on, for instance, `../godement-index/chapter20.dk`.

## Symbol tables (in the projects)

The check of a3d0298 (`RunInformath -base=X.dk X.dkgf`) leaves, over
hott-llm and godement-index:

* hott-llm `section02.dkgf`, `section04.dkgf`: `Pi` and `Sigma` have forms
  like `$\prod_{(#2 : #1)} #3$`, using #3 as the bound variable and body of
  their second argument, which is one argument: ARGUMENT OUT OF RANGE.
* godement-index `partI.dkgf`: `continuum_hypothesis`, a Prop, is worded as a
  name ("the continuum hypothesis"): MISMATCHING TYPES.
* A false alarm of the check: hott-llm `isMultipleOfGcd`, whose value type
  `Fam Nat` is a function type only by its definition, which the check does
  not unfold.
* The check does not compare argument categories (almost all GF argument
  slots are Exp), nor which argument a #k shows.

## Found by the trial of autoformalize/ (5182766)

* A hypothesis of function type, `(R : Elem A -> Elem A -> Prop)`, reads
  "Let $R$ be a function from elements $R$ of $A$ and elements of $A$ to
  propositions" (a variable clash), and `-to-symbolic-latex` shows the
  constant as `\mathrm{symmetricF}(A, R, R)`, with R twice. To see it:

      def symmetricF : (A : Set) -> (R : Elem A -> Elem A -> Prop) -> Prop :=
        A => R => forall A (x => forall A (y => if (R x y) (R y x))).

  with `symmetricF : "#2 is symmetric"`, read back with
  `-base=share/baseconstants.dk`.
* `-proof-text` can leave out a hypothesis of the statement: the proof of a
  statement over a set A and a relation R on it began with R, without "Let
  $A$ be a set".
* A higher-order argument of a constant, `setOf X (y => R y)`, reads "the
  set of the function that maps $y$ to $R$ applied to $y$ in $X$"
  (godement-index `setOf_char`).
* No example form parses for a binder over a type: "the union of #2 where
  $x$ is an element of #1" and the like. The binder forms that parse are
  "the <Noun> #1 of $x$" and "the <Noun> of #3 where $x$ ranges from #1 to #2".

## Agreement of "such that" (22 October)

`such_that_Subj` ("such that", "tel que", "takový, že") cannot agree with
the noun. bf2e121 added `such_A` and `suchThatCN` (`Utilities`), now
defined in each language (`Utilities*.gf`, also `next/`): an agreeing AP in
Eng, Fre, Swe, Cze (with ", že") and Pol; "derart, dass" after the noun in
Ger, which does not inflect; "sellainen" before the noun and ", että" after
it in Fin (a shared AP version put the whole clause before the noun in Ger
and Fin). It is used in `CoreExistProp`, `SuchThatHypo`, `ExistUniqueProp`,
`TheUniqueExp` and `BareSuchThatKind`. Still open:

* `ExistProp`, `ExistNoProp` (MathExtensions) and Mizar's
  `FraenkelComprehensionTerm` still use `such_that_Subj`, after an NP that
  is already built (`[ArgKind] = {sg, neg, pl : NP}`). These are the
  readings that NLG prefers, so most existentials still come out wrong:
  Fre "un nombre naturel $n$ tel que" is right only by chance ("une matrice
  $M$ tel que", "des nombres naturels $m$ et $n$ tel que"); Cze "přirozené
  číslo $n$, takový, že" (the problem of slide 15 of
  `doc/claude-commits-slides.pdf`); Pol "liczba naturalna $n$, taki, że";
  Swe "så att" and Fin "siten että" mean "so that", "in such a way that".
  Ger is right, as its `such_that_Subj` is now "derart, dass".
  A first attempt kept the last kind of the list in `[ArgKind]` (its CN, the
  NPs of the others): `-v` showed `ConsArgKind` grow from 3538944 to
  50096498540544 concrete functions, French took 10 minutes instead of 25 s,
  and English got worse, so it was reverted. A lighter way: keep the
  original NPs and only the CN of a single kind, agreeing only when the
  list has one kind, the common case.
* In English, `ExistProp` writes ", such that" and `CoreExistProp` "such
  that": in `naproche-interpret`, the readings of `PropositionNeqWitness`
  doubled from 72 to 144, differing only in that comma.
* `SuchThatKind` (MathCore) and `DeclSuchThatKind` (GodementSyntax) keep
  `such_that_Subj` in the adverb of the kind: with `suchThatCN` in the CN, a
  variable added to the kind afterwards (`identKindCN`) came after the
  clause, "a reflection $g$ such that $g$ is left $x_0$ such that ...". The
  `Kind = {cn, adv}` split would need a place for the variable before the
  clause.
* Fre: "**Il existent** des nombres naturels $m$ et $n$": the impersonal
  "il existe" must stay singular, but `ExistsNP` agrees with the NP.
* `such_that_Subj` is also used in `forthel/` (not changed), and
  `NaprocheEng` / `next/NaprocheFin` write "such that" as a string (in
  Finnish too).

## Found by mathlib-informath (21-22 October)

* Fre: "pour tous **des** nombres naturels" (should be "les"), "il n'est pas
  le cas que" (a calque; "il n'est pas vrai que"), "une relation
  d'**équivalences**".
* Eng: "does not have **a** upper bound".
* A proposition inside a macro argument loses its notation:
  `\mathrm{Lt}(n, x \times x)` inside if-then-else.
* Multi-letter variables (`lt`, `self`, `motive`) are shown as `\mathrm{lt}`,
  like constants; a constant used unapplied, as `Leq`, gives
  `\mathrm{Leq}(x_2, x_3)`.
* `$` inside a quoted verbal form splits a symbol table entry (`splitEntry`
  in `BuildConstantTable.hs`), which rules out the binder shapes ("the set
  #1 of $x$") in tables; the symbol-table skill of `autoformalize/`
  recommends them.
* Proof texts: a lemma or constructor used unapplied gives "... elements
  $\mathrm{irrefl}$ of $\mathrm{Proof}$ applied to the proposition that
  ..."; bound variables are renumbered ($x_4$, $x_5$).
* `proof2dedukti` does not cover proofs, so `-json` crashes on them without
  `-no-back-to-dedukti`.
* Partial French modules: `NaprocheWordsFre`, `GodementWordsFre`,
  `GodementExamplesFre` have only the words mathlib-informath uses.

## Regression tests

`python3 regression/run.py`: 46 PASS, 2 XFAIL (fermat: conflicting profile
information; bind: test/bind.dk does not parse), and exx-swe FAIL, exx-ger
SKIP, as `share/InformathFull.pgf` now has only English and French
(mathlib-informath); build it with all languages (`make full_grammar`) to
test those.

## Elsewhere

* Rijke's Informath issues, with examples: `../hott-llm/NOTES.md`,
  "Informath: issues to fix" (Curry-Howard theorems read as definitions,
  kind-level definitions, application of variables, ...).
* The Hypo and ArgKind syntax: `todo/hypos.txt`.
