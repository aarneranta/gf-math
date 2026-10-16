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

## Regression tests

`python3 regression/run.py`: 49 PASS, 1 NEW (exx-ger, which has no expected
output yet), 2 XFAIL (fermat: conflicting profile information; bind:
test/bind.dk does not parse).

## Elsewhere

* Rijke's Informath issues, with examples: `../hott-llm/NOTES.md`,
  "Informath: issues to fix" (Curry-Howard theorems read as definitions,
  kind-level definitions, application of variables, ...).
* The Hypo and ArgKind syntax: `todo/hypos.txt`.
