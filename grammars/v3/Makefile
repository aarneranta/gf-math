all: Core.pgf Dedukti

.PHONY: all Core.pgf Dedukti Agda

Core.pgf:
	gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun,Label CoreEng.gf

Dedukti:
	cd typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make



