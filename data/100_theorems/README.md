# The 100 Theorems

According to Freek Wiedijk, https://www.cs.ru.nl/~freek/100/ 


## Formalizations in Naproche

https://github.com/naproche/naproche/blob/master/examples/100_theorems.ftl.tex

contains 10 of the 100

## A first test with GF

The grammars in [100thm](../../grammars/100thm/) extend the big WikiMath grammar with constructs
and words that are either
- not found in Forthel.gf
- not found in a particular language


The test presupposes a compilation of the `100thm` grammars:
```
  gf -make Ext100Math???.gf
```
After that, we run a script in the current directory:
```
  cat modif100lexed.txt | python3 translatex.py -latex \
  ../../grammars/100thm/Ext100Math.pgf Ext100MathEng TERMINDEX.json >body.tex
```
We append this file to a preamble:
```
  cat preamble.tex body.tex >ex100.tex
```
Before doing so, you can use this file to look for unknown words that have to be fixed in
`Ext100MathXXX.gf` for your language `XXX`.

Running pdflatex
on this finally gives us [./ex100.pdf](./ex100.pdf).
The packages and macros in `preamble.tex` are from the original Naproche document.

You can also run `translatex.py -json` with the same input and arguments to get information
about the trees and other details.


## Preparing the text

You first have to extract the ForTheL parts and then lex into space-separated tokens.
The lexing can be done in the GF shell as follows:
```
  > rf -file=FILE -lines | ps -unlextext -lines | wf -file=LEXEDFILE
```
After this, you can replace terms with INDEXEDTErm tokens and create a term index by running the
[index_latex_terms](../index_latex_terms.py) script on LEXEDFILE.



