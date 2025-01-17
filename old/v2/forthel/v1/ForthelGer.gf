-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelGer of Forthel =

  ForthelTermsAscii **
  ForthelFunctor - [pluralNP, NamesAssumption,
  StatementAssumption, possessAdv, ApposTermSymb] with
    (Syntax=SyntaxGer),
    (Symbolic=SymbolicGer),
    (Extend=ExtendGer),
    (Grammar=GrammarGer),
    (Markup=MarkupGer)

** open

  ParadigmsGer,
  (P=ParadigmsGer),
  (M=MakeStructuralGer),
  (R=ResGer),
  (I=IrregGer),
  Prelude
  
in {

-- functor exceptions
lin
  NamesAssumption names classnoun =
    mkText 
      (mkS (mkCl we_NP assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv))))) ;

  StatementAssumption stat =
    mkText 
      (mkS (mkCl we_NP assume_VS stat)) ;

  ApposTermSymb primc name = {
    np = mkNP the_Det (mkCN (mkCN primc.cn (symb name)) primc.adv) ;
    sym = name
    } ;

oper
  pluralNP : NP -> NP = \np -> np ** {a = R.AgPl R.P3} ;

  possessAdv : NP -> Adv = \np -> mkAdv genPrep np ;

-- words etc

  denote_V2 : V2 =
    mkV2 (mkV "bedeuten") ;

  any_Quant = a_Quant ; ---- TODO
  each_Det = every_Det ; ---- TODO
  such_that_Subj = M.mkSubj "so dass" ;

  iff_Conj = M.mkConj [] "wenn und nur wenn" singular ;

  equal_A2 : A2 = mkA2 (mkA "gleich") with_Prep ;

  assume_VS : VS = mkVS (prefixV "an" I.nehmen_V) ;

  then_Adv : Adv = P.mkAdv "dann" ;

  let_Str : Str = "sei" ; --- seien ?
  
}
