all: Informath.pgf Dedukti Agda Lean RunInformath

.PHONY: all Informath.pgf Dedukti Agda Lean RunInformath demo

Informath.pgf:
	cd grammars ; gf --make -output-format=haskell -haskell=lexical --haskell=gadt -lexical=Name,Noun,Adj,Rel,Fun,Label,Const,Oper,Compar,Set,Coercion --probs=Informath.probs InformathEng.gf InformathSwe.gf
	sed -i '' 's/Monad\.Identity/Monad\.Identity\nimport Control\.Monad/' grammars/Informath.hs
	ln -sf grammars/Informath.hs

Dedukti:
	cd typetheory ; bnfc -m -p Dedukti --haskell-gadt Dedukti.bnf ; make
	ln -sf typetheory/Dedukti

Agda:
	cd typetheory ; bnfc -m -p Agda --haskell-gadt Agda.bnf ; make
	ln -sf typetheory/Agda

Lean:
	cd typetheory ; bnfc -m -p Lean --haskell-gadt Lean.bnf ; make
	ln -sf typetheory/Lean

RunInformath: 
	ghc -package gf RunInformath.hs

demo:
	./RunInformath -lang=Swe test/exx.dk
	./RunInformath -lang=Eng test/exx.dk
	./RunInformath -lang=Eng test/exx.dk >exx.txt
	./RunInformath -lang=Eng exx.txt
	cat BaseConstants.dk test/exx.dk >bexx.dk
	dk check bexx.dk
	./RunInformath -to-agda test/exx.dk >exx.agda
	agda --prop exx.agda
	./RunInformath -to-lean test/exx.dk >exx.lean
	cat BaseConstants.lean exx.lean >bexx.lean   
	lean bexx.lean    

