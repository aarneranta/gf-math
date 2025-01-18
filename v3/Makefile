all: Core.pgf Dedukti

.PHONY: all Core.pgf Forthel.pgf Dedukti Agda Lean

Core.pgf:
	cd Core ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun,Label CoreEng.gf

Forthel.pgf:
	cd forthelplus ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun,Label ForthelEng.gf

Dedukti:
	cd typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make

Lean:
	cd typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make

