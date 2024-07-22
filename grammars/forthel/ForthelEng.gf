-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelEng of Forthel =

  ForthelTermsAscii **
  ForthelFunctor with
    (Syntax=SyntaxEng),
    (Symbolic=SymbolicEng),
    (Extend=ExtendEng),
    (Grammar=GrammarEng),
    (Markup=MarkupEng)

** open

  (P=ParadigmsEng),
  (M=MakeStructuralEng),
  (R=ResEng),
  (I=IrregEng),
  Prelude
  
in {

oper
  denote_V2 : V2 = P.mkV2 "denote" | P.mkV2 I.stand_V for_Prep ; --- allow variants ?

  s2np : S -> NP = \s -> symb (mkSymb (mkUtt s).s) ; --- hack; Forthel is not quite grammatical here

  parenthS : S -> S = \s -> Markup.MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  any_Quant = P.mkQuant "any" "any" ;
  each_Det = M.mkDet "each" ;
  such_that_Subj = P.mkSubj "such that" ;

  iff_Conj = P.mkConj "iff" ;

  equal_A2 : A2 = P.mkA2 (P.mkA "equal") to_Prep ;
}
