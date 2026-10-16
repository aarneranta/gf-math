# Autoformalization experiment

This directory is an experiment in formalizing a mathematical text written in
LaTeX: its content is formalized in Dedukti, each formal constant is given an
Informath symbol table entry that says it in words and in notation as close
to the text as possible, and the result is presented as a web page and as
proof documents. It follows the Godement project (`godement-index`) and the
Rijke project (`hott-llm`), whose notes are worth reading for hard cases.

## What to produce

| file | what it is | kept correct by |
|---|---|---|
| `base.dk` | the foundation (`scripts/init.sh`) | `scripts/check.sh` |
| `secNN.dk` | the formalization of unit NN (a chapter, or a section) of the source | `scripts/check.sh` |
| `secNN.dkgf` | its symbol table: verbal and symbolic forms of its constants | `scripts/check-table.py` |
| `base.dkgf` | entries for what `base.dk` adds, and overrides of Informath's base | `scripts/check-table.py` |
| `<Name>Words.gf` in informath | words the grammar lacked, if any | the `grammar-extension` skill |
| `secNN-informal.tsv`, `-symbolic.tsv`, `-closest.tsv` | Informath's readings | `scripts/build-informal.py` |
| `proofs/secNN.pdf` | the proofs, as numbered lines of English | `scripts/build-proofs.sh` |
| `<name>-index.html` | every item of the source with its formalization and readings | `scripts/build-index.py` |
| `NOTES.md` | the encoding decisions, the state, and the issues found in Informath | you |

`<name>` is the `name` of `experiment.json`. Its other fields: `source`, the
main LaTeX file (its `\input`s are followed); `title`; `tag`, the word used in
the tags (default `DOC`); `eta`, true if conversion must be modulo eta; and
`macros`, MathJax definitions of document macros the scripts cannot take over.

## Setup (once)

    export INFORMATH_ROOT=<the informath checkout>   # its grammar, base, and RunInformath
    which RunInformath dk xelatex                    # all three must be on the PATH
    scripts/init.sh            # base.dk: Informath's base constants + natural deduction
    scripts/init.sh own        # or: a foundation of the experiment's own
    python3 scripts/extract-items.py                 # items.json: the source's items

Choose the foundation with the user if it is not obvious. Ordinary
mathematics (sets, numbers, algebra) takes the default, which brings `Nat`,
`Int`, `Real`, `plus`, `times`, `Eq`, the connectives and quantifiers with
their readings, and the rules of `base/nd.dk` for proofs. A type theory, or a
text that builds everything from its own primitives, needs `own` (see
`hott-llm/NOTES.md`, "The encoding").

`RunInformath` must be built from the checkout `INFORMATH_ROOT` points to
(`stack install` there), or its grammar and its binary disagree.

## The loop

Work one unit at a time, in order, and finish each before the next:

1. **Read** the unit in the source, and `items.json` for its items (their
   numbers, labels and kinds). Plan the constants: the notions it defines,
   the results it states, and what they rest on. `grep` the base and the
   earlier units before naming a constant.
2. **Formalize** in `secNN.dk`, every declaration tagged with its item (see
   the conventions below), and run `scripts/check.sh` until it passes.
3. **Word** the new constants in `secNN.dkgf` -- load the `symbol-table`
   skill -- and run `python3 scripts/check-table.py secNN.dkgf` until it
   reports no errors. A missing word: the `grammar-extension` skill.
4. **Read back**: `python3 scripts/build-informal.py`, then `-symbolic`, then
   `-closest`, which replaces the plain readings in `secNN-informal.tsv` by
   the closest ones (keeping the plain reading where there is nothing to
   compare). Read the sentences beside the source. Where they differ, improve
   the entries, the names of the hypotheses, or the shape of the statement,
   and repeat from 2 or 3.
5. **Present**: `scripts/build-proofs.sh`, then `python3 scripts/build-index.py`
   (in this order, as the index links the proof documents).
6. **Record** in `NOTES.md`: what the unit formalizes and how, what is stated
   without proof, what reads badly and why, and Informath issues.

Report to the user after each unit, and stop where they asked to stop. All
readings and the index must be rebuilt together after any symbol table
change, as the macro names depend on all the tables; the scripts do that.

## Formalization conventions

* **Faithful to the source.** State what the text states, with its
  hypotheses, no stronger and no weaker; define what it defines, the way it
  defines it. Do not prove something else because it is easier. Where the
  formalization has to depart from the text, say so in a comment and in
  NOTES.md.
* **Kinds of declarations.** A defined notion is a `def`. A result is a
  constant of type `Proof (...)`: with a proof (`def thm : ... := term`,
  checked by Dedukti, written out by `build-proofs.sh`) where the proof is
  within reach of the rules in `base.dk`, or stated without one. A primitive
  notion of the text, or an axiom it assumes, is a declaration without a body.
  Never add an axiom to make a proof go through without saying so: state the
  arithmetic or the lemma the proof uses as a separate, tagged declaration,
  as `example/sec01.dk` does with `times_one`.
* **Proofs** use the introduction and elimination rules of `base/nd.dk`
  (`andI`, `ifI`, `ifE`, `forallI`, `forallE`, `existsI`, `existsE`, ...).
  Each application of a rule becomes a line of the proof text; a stated lemma
  used in a proof appears as an assumption line.
* **Tags.** Put `(; //DOC:<label> ;)` (with the experiment's tag for `DOC`)
  on the line directly above each declaration: the item's `\label` if it has
  one, else its number as `items.json` gives it (`2.3`). A section number
  (`2`, `2.1`) attaches a declaration to the running text of that section.
  An auxiliary declaration gets the tag of the item that needs it. Untagged
  declarations do not appear in the index.
* **Layout.** A declaration starts at the beginning of a line with its name
  and a colon on the same line (`def divides : ...`), and ends with a blank
  line: the index takes everything from the tag to the next blank line.
  Comments `(; ... ;)` go above the tag or at the end.
* **Names.** Use the source's names where they are identifiers
  (`isNormal`, `card`), and the source's letters for the variables of the
  types (`(G : Elem Obj) -> (H : Elem Obj) -> ...`): the readings use them.
  Avoid Dedukti keywords (`injective` is one) and names of the base
  (`even`, `union`, `prime`, ...: `grep` `base.dk`).
* **Bound variables.** The names of λ-bound variables (`x => ...`) in
  definitions and proof terms appear in the readings and the proof texts
  (`y0` comes out as $y_{0}$): use the source's letters for them too.
* **Relations and predicates as variables.** A hypothesis of function type,
  `(R : Elem A -> Elem A -> Prop)`, reads badly ("Let $R$ be a function from
  elements $R$ of $A$ and elements of $A$ to propositions", "$R$ holds for
  $x$ and $y$"). Where the source quantifies over relations (or predicates,
  or families), declare a type of them and an application constant instead:

      Rel : (A : Set) -> Set.                                       (; "binary relation on #1" ;)
      holds : (A : Set) -> (R : Elem (Rel A)) -> (x : Elem A) -> (y : Elem A) -> Prop.
                                                                    (; "#3 is related to #4 by #2" | $#3 \mathrel{#2} #4$ ;)

  so that "Let $R$ be a binary relation on $A$" and "$x \mathrel{R} y$" come
  out as in the source. A higher-order argument of a constant
  (`setOf X (y => P y)`) reads as "the function that maps $y$ to ..."
  unless the constant's form is a binder form (see the `symbol-table`
  skill); give such a constant a symbolic form in any case.
* **Size.** A statement that is too big for Informath to word (`TIMED OUT`
  in the read-back) is to be split into the parts the text itself has --
  a), b), c), or the clauses of a long theorem -- each tagged with the item.

## Getting the readings close to the source

The English readings are generated from the formal statements and the
symbol tables; they come close to the source when (a) the entries use the
source's words and notation, (b) the statement has the source's structure
(its hypotheses, the order of its quantifiers, its definitions as
definitions), and (c) the variables have the source's names. `-closest`
compares up to 50 readings of each declaration with the text of its item and
keeps the closest; `secNN-closest.tsv` gives the similarity (0 to 1), which
the index shows. A declaration tagged with a section is compared with the
section's running text. A similarity below about 0.3 is worth a look; above
about 0.6 is close. Informath puts all hypotheses first ("Let $x$ and $y$ be
elements of $A$. Assume that ...") and says "if ... then" for "implies", so
statements written with a trailing "for all $x$" rarely score much higher
than 0.4.

## Informath: what to expect, and what to report

* The readings name every hypothesis ("Let $a$ be a natural number. Assume
  that ..."), and definitions come out as "Then $a$ divides $b$, if ...".
* A proof document heads each proof with its Dedukti type, not an English
  statement, and its first lines may leave out hypotheses that the steps do
  not use (a type `A` of the statement need not get its "Let $A$ be a set").
* A constant without a symbolic form reads as `\mathrm{c}(x)` in formulas.
* A unit read back as a whole has 120 s, a declaration 30 s; what runs out
  of time is reported as TIMED OUT. One without any wording is retried with
  `-force-symbolic` (FORCED SYMBOLIC) and reads as formulas.

When Informath seems wrong -- a crash, an ill-formed or misleading reading, a
false alarm of `check-table.py` -- write a minimal example (a few lines of
`.dk` and `.dkgf`) and the command into `NOTES.md` under "Informath issues",
and tell the user. Do not change Informath's Haskell code or its core
grammar without the user's agreement; adding words in the experiment's own
words module is fine.

## Working with the user

* Commit only when the user asks, and never push.
* The source is the reference: when unsure what the text means, or which of
  two encodings to choose, ask rather than guess, and record the decision.
* `example/` is a complete small experiment (`divisibility.tex`): run its
  pipeline to see what each step produces.
