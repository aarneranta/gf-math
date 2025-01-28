all: ForthelPlus.pgf Dedukti

.PHONY: all Informath.pgf Dedukti Agda Lean RunInformath

Informath.pgf:
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun,Label,Const,Oper,Compar,Set,Coercion --probs=Informath.probs InformathEng.gf
	sed -i '' 's/Monad\.Identity/Monad\.Identity\nimport Control\.Monad/' grammars/Informath.hs

Dedukti:
	cd typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make

Agda:
	cd typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make

Lean:
	cd typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make

RunInformath:
	ghc -package gf RunInformath.hs
