-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelEng of Forthel =

  ForthelTermsAscii **
  ForthelFunctor - [negPol] with
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
-- functor exceptions

  negPol = Extend.UncontractedNeg ;

-- words etc

  denote_V2 : V2 =
    P.mkV2 "denote" | P.mkV2 I.stand_V for_Prep ; --- allow variants ?

  any_Quant = P.mkQuant "any" "any" ;
  each_Det = M.mkDet "each" ;
  such_that_Subj = P.mkSubj "such that" ;

  iff_Conj = P.mkConj "iff" ;

  equal_A2 : A2 = P.mkA2 (P.mkA "equal") to_Prep ;
}
