all: Core.pgf Dedukti

.PHONY: all Core.pgf

Core.pgf:
	gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun CoreEng.gf

Dedukti:
	cd typetheory
	bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf
	cd ..



