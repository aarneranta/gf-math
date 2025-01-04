all: Core.pgf Dedukti

.PHONY: all Core.pgf

Core.pgf:
	gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Ident,Kind CoreEng.gf

Dedukti:
	bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf

