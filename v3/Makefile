all: ForthelPlus.pgf Dedukti

.PHONY: all ForthelPlus.pgf Dedukti Agda Lean

ForthelPlus.pgf:
	cd forthelplus ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun,Label ForthelPlusEng.gf

Dedukti:
	cd typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make

Lean:
	cd typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make

