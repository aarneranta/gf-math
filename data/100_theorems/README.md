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
  cat modif100lexed.txt | python3 ../../grammars/forthel/test_file.py \
  ../../grammars/100thm/Ext100Math.pgf Ext100MathEng TERMINDEX.json
```
Piping and redirecting this with
```
  | grep -v TREE > ex.tmp
```
gives us a file that we can unlex in GF:
```
  > rf -file=ex.tmp -lines | ps -unlextext -lines | wf -file=uex.tmp
```
Gluing the resulting lines into [./ex100.tex](./ex100.tex) and running pdflatex
on this finally gives us [./ex100.pdf](./ex100.pdf).
The packages and macros in the .tex fila are from the original Naproche document.

