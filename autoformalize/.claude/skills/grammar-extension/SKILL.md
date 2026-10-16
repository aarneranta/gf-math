---
name: grammar-extension
description: Add words to Informath's GF grammar when a symbol table entry needs a word the grammar lacks (check-table.py: CANNOT PARSE, and RunInformath -find-gf shows nothing for the word). Covers the experiment's own words module in informath/grammars, registering it, rebuilding the grammar and RunInformath, and checking. Use only for words; new example shapes or syntax are changes to discuss with the user first.
---

# Extending Informath's grammar with words

The grammar lives in the informath checkout that `INFORMATH_ROOT` points to.
An experiment adds its words in a module of its own, as Godement's and
Rijke's projects did (`GodementWords.gf`, `HottWords.gf`):

    $INFORMATH_ROOT/grammars/<Name>Words.gf      abstract: fun lines
    $INFORMATH_ROOT/grammars/<Name>WordsEng.gf   English:  lin lines

`<Name>` is the experiment's name, capitalized (`DivisibilityWords`). This
changes the shared informath checkout: tell the user what you added, and
leave committing it to them.

## 1. Check that the word is really missing

    echo "coprime equipotent" | RunInformath -find-gf

A word with nothing after `:` is missing. Also try its other forms (a noun's
singular, a verb's infinitive) and the compound as one word
(`two-sided`): `-find-gf` looks words up as they are written. Search the
lexicon modules before adding, since a second declaration of the same
function breaks the build ("cannot unify the information"):

    grep -l "coprime_Adj" $INFORMATH_ROOT/grammars/*.gf

## 2. Write the module (first time)

```
abstract DivisibilityWords = Categories ** {
-- words of divisibility.tex
fun coprime_Adj : Adj ;
fun multiple_Noun : Noun ;
fun divide_Verb2 : Verb2 ;
}
```

```
concrete DivisibilityWordsEng of DivisibilityWords = CategoriesEng **
open UtilitiesEng, IrregEng, ParadigmsEng in {
lin coprime_Adj = mkAdj "coprime" ;
lin multiple_Noun = mkNoun "multiple" ;
lin divide_Verb2 = mkVerb2 (mkVerb "divide") ;
}
```

* Nouns: `mkNoun "group"`, irregular plural `mkNoun "matrix" "matrices"`.
  A compound noun is usually better built from known words in the entry
  itself ("normal subgroup" parses if "normal" and "subgroup" are known).
* Adjectives: `mkAdj "even"`; with a hyphen `mkAdj "two-sided"`, and then the
  function name without the hyphen: `two_sided_Adj` (quoted names such as
  `'two-sided_Adj'` are printed raw by the generated Haskell).
* Verbs: `mkVerb "converge"` (Verb), `mkVerb2 (mkVerb "divide")` (Verb2);
  irregular verbs from `IrregEng` (`mkVerb2 lie_V`).
* Atomic words are what the entries need: Noun, Adj, Verb, Verb2. The
  shapes ("#1 is a <Noun> of #2", "the <Noun> of #1") combine them with the
  prepositions; do not add Noun2, Fun2 etc. for a single entry.

## 3. Register it (first time)

Add the module to both lists, after the last module:

    $INFORMATH_ROOT/grammars/UserExtensions.gf      ..., GodementExamples, DivisibilityWords
    $INFORMATH_ROOT/grammars/UserExtensionsEng.gf   ..., GodementExamplesEng, DivisibilityWordsEng

## 4. Rebuild and install

    cd $INFORMATH_ROOT
    make share/InformathEng.pgf      # the grammar, and src/Informath.hs from it
    stack install                    # RunInformath, which compiles Informath.hs in

`stack install` is needed every time: RunInformath contains Haskell code
generated from the grammar. A GF error names the file and line. Then run the
regression tests, which must still pass:

    python3 regression/run.py

## 5. Check

    echo "coprime" | RunInformath -find-gf
    python3 scripts/check-table.py sec02.dkgf    # in the experiment

Record the words added in the experiment's NOTES.md.
