-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelFre of Forthel =

  ForthelTermsAscii,
  LatexTermsTex **
  ForthelFunctor - [
    pluralNP, PredicateSynonym, NamesAssumption, StatementAssumption
    ] with
    (Syntax=SyntaxFre),
    (Symbolic=SymbolicFre),
    (Extend=ExtendFre),
    (Grammar=GrammarFre)
---    (Markup=MarkupFre)

** open

  ParadigmsFre,
  (P=ParadigmsFre),
  (M=MakeStructuralFre),
  (R=ResFre),
  (I=IrregFre),
  Prelude
  
in {

-- functor exceptions
lin
  NamesAssumption names classnoun =
    mkText 
      (lets_Utt (mkVP assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv)))))
      fullStopPunct ;
      
  StatementAssumption stat =
    mkText 
      (lets_Utt (mkVP assume_VS stat)) fullStopPunct ;
oper
  pluralNP : NP -> NP = \np -> np ** {n=plural} ;

-- words etc

  denote_V2 : V2 =
    mkV2 (mkV "signifier") ;

  any_Quant = a_Quant ; ---- TODO
  each_Det = every_Det ; ---- TODO
  such_that_Subj = M.mkSubj "tel que" ;

  iff_Conj = M.mkConj [] "si et seulement si" singular ;

  equal_A2 : A2 = mkA2 (mkA "égal") with_Prep ;

  assume_VS : VS = mkVS (mkV "supposer") ;

  then_Adv : Adv = P.mkAdv "alors" ;

  let_Str : Str = "soit" ; --- seien ?
  
}
